/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "frontend/BytecodeCompiler.h"

#include "jscntxt.h"
#include "jsscript.h"

#include "asmjs/AsmJS.h"
#include "builtin/ModuleObject.h"
#include "frontend/BytecodeEmitter.h"
#include "frontend/FoldConstants.h"
#include "frontend/NameFunctions.h"
#include "frontend/Parser.h"
#include "vm/GlobalObject.h"
#include "vm/TraceLogging.h"

#include "jsobjinlines.h"
#include "jsscriptinlines.h"

#include "vm/EnvironmentObject-inl.h"

using namespace js;
using namespace js::frontend;
using mozilla::Maybe;

class MOZ_STACK_CLASS AutoCompilationTraceLogger
{
  public:
    AutoCompilationTraceLogger(ExclusiveContext* cx, const TraceLoggerTextId id,
                               const ReadOnlyCompileOptions& options);

  private:
    TraceLoggerThread* logger;
    TraceLoggerEvent event;
    AutoTraceLog scriptLogger;
    AutoTraceLog typeLogger;
};

// The BytecodeCompiler class contains resources common to compiling scripts and
// function bodies.
class MOZ_STACK_CLASS BytecodeCompiler
{
  public:
    // Construct an object passing mandatory arguments.
    BytecodeCompiler(ExclusiveContext* cx,
                     LifoAlloc* alloc,
                     const ReadOnlyCompileOptions& options,
                     SourceBufferHolder& sourceBuffer,
                     HandleScope enclosingScope,
                     TraceLoggerTextId logId);

    // Call setters for optional arguments.
    void maybeSetSourceCompressor(SourceCompressionTask* sourceCompressor);
    void setSourceArgumentsNotIncluded();

    JSScript* compileGlobalScript(ScopeKind scopeKind);
    JSScript* compileEvalScript(HandleObject environment, HandleScope enclosingScope);
    ModuleObject* compileModule();
    bool compileFunctionBody(MutableHandleFunction fun, Handle<PropertyNameVector> formals,
                             GeneratorKind generatorKind);

    ScriptSourceObject* sourceObjectPtr() const;

  private:
    JSScript* compileScript(HandleObject environment, SharedContext* sc);
    bool checkLength();
    bool createScriptSource();
    bool maybeCompressSource();
    bool canLazilyParse();
    bool createParser();
    bool createSourceAndParser();
    bool createScript();
    bool createEmitter(SharedContext* sharedContext);
    bool handleParseFailure(const Directives& newDirectives);
    bool prepareTree(ParseNode** pn);
    bool deoptimizeArgumentsInEnclosingScripts(JSContext* cx, HandleObject environment);
    bool maybeSetDisplayURL(TokenStream& tokenStream);
    bool maybeSetSourceMap(TokenStream& tokenStream);
    bool maybeSetSourceMapFromOptions();
    bool maybeCompleteCompressSource();

    AutoCompilationTraceLogger traceLogger;
    AutoKeepAtoms keepAtoms;

    ExclusiveContext* cx;
    LifoAlloc* alloc;
    const ReadOnlyCompileOptions& options;
    SourceBufferHolder& sourceBuffer;

    RootedScope enclosingScope;
    bool sourceArgumentsNotIncluded;

    RootedScriptSource sourceObject;
    ScriptSource* scriptSource;

    Maybe<SourceCompressionTask> maybeSourceCompressor;
    SourceCompressionTask* sourceCompressor;

    Maybe<Parser<SyntaxParseHandler>> syntaxParser;
    Maybe<Parser<FullParseHandler>> parser;

    Directives directives;
    TokenStream::Position startPosition;

    RootedScript script;
    Maybe<BytecodeEmitter> emitter;
};

AutoCompilationTraceLogger::AutoCompilationTraceLogger(ExclusiveContext* cx,
        const TraceLoggerTextId id, const ReadOnlyCompileOptions& options)
  : logger(cx->isJSContext() ? TraceLoggerForMainThread(cx->asJSContext()->runtime())
                             : TraceLoggerForCurrentThread()),
    event(logger, TraceLogger_AnnotateScripts, options),
    scriptLogger(logger, event),
    typeLogger(logger, id)
{}

BytecodeCompiler::BytecodeCompiler(ExclusiveContext* cx,
                                   LifoAlloc* alloc,
                                   const ReadOnlyCompileOptions& options,
                                   SourceBufferHolder& sourceBuffer,
                                   HandleScope enclosingScope,
                                   TraceLoggerTextId logId)
  : traceLogger(cx, logId, options),
    keepAtoms(cx->perThreadData),
    cx(cx),
    alloc(alloc),
    options(options),
    sourceBuffer(sourceBuffer),
    enclosingScope(cx, enclosingScope),
    sourceArgumentsNotIncluded(false),
    sourceObject(cx),
    scriptSource(nullptr),
    sourceCompressor(nullptr),
    directives(options.strictOption),
    startPosition(keepAtoms),
    script(cx)
{
    MOZ_ASSERT(sourceBuffer.get());
}

void
BytecodeCompiler::maybeSetSourceCompressor(SourceCompressionTask* sourceCompressor)
{
    this->sourceCompressor = sourceCompressor;
}

void
BytecodeCompiler::setSourceArgumentsNotIncluded()
{
    sourceArgumentsNotIncluded = true;
}

bool
BytecodeCompiler::checkLength()
{
    // Note this limit is simply so we can store sourceStart and sourceEnd in
    // JSScript as 32-bits. It could be lifted fairly easily, since the compiler
    // is using size_t internally already.
    if (sourceBuffer.length() > UINT32_MAX) {
        if (cx->isJSContext())
            JS_ReportErrorNumber(cx->asJSContext(), GetErrorMessage, nullptr,
                                 JSMSG_SOURCE_TOO_LONG);
        return false;
    }
    return true;
}

bool
BytecodeCompiler::createScriptSource()
{
    if (!checkLength())
        return false;

    sourceObject = CreateScriptSourceObject(cx, options);
    if (!sourceObject)
        return false;

    scriptSource = sourceObject->source();
    return true;
}

bool
BytecodeCompiler::maybeCompressSource()
{
    if (!sourceCompressor) {
        maybeSourceCompressor.emplace(cx);
        sourceCompressor = maybeSourceCompressor.ptr();
    }

    if (!cx->compartment()->behaviors().discardSource()) {
        if (options.sourceIsLazy) {
            scriptSource->setSourceRetrievable();
        } else if (!scriptSource->setSourceCopy(cx, sourceBuffer, sourceArgumentsNotIncluded,
                                                sourceCompressor))
        {
            return false;
        }
    }

    return true;
}

bool
BytecodeCompiler::canLazilyParse()
{
    return options.canLazilyParse &&
           !(enclosingScope && enclosingScope->hasEnclosing(ScopeKind::NonSyntactic)) &&
           !cx->compartment()->behaviors().disableLazyParsing() &&
           !cx->compartment()->behaviors().discardSource() &&
           !options.sourceIsLazy &&
           !cx->lcovEnabled();
}

bool
BytecodeCompiler::createParser()
{
    if (canLazilyParse()) {
        syntaxParser.emplace(cx, alloc, options, sourceBuffer.get(), sourceBuffer.length(),
                             /* foldConstants = */ false, (Parser<SyntaxParseHandler>*) nullptr,
                             (LazyScript*) nullptr);

        if (!syntaxParser->checkOptions())
            return false;
    }

    parser.emplace(cx, alloc, options, sourceBuffer.get(), sourceBuffer.length(),
                   /* foldConstants = */ true, syntaxParser.ptrOr(nullptr), nullptr);
    parser->sct = sourceCompressor;
    parser->ss = scriptSource;
    if (!parser->checkOptions())
        return false;

    parser->tokenStream.tell(&startPosition);
    return true;
}

bool
BytecodeCompiler::createSourceAndParser()
{
    return createScriptSource() &&
           maybeCompressSource() &&
           createParser();
}

bool
BytecodeCompiler::createScript()
{
    script = JSScript::Create(cx, options,
                              sourceObject, /* sourceStart = */ 0, sourceBuffer.length());
    return script != nullptr;
}

bool
BytecodeCompiler::createEmitter(SharedContext* sharedContext)
{
    BytecodeEmitter::EmitterMode emitterMode =
        options.selfHostingMode ? BytecodeEmitter::SelfHosting : BytecodeEmitter::Normal;
    emitter.emplace(/* parent = */ nullptr, parser.ptr(), sharedContext, script,
                    /* lazyScript = */ nullptr, options.lineno, emitterMode);
    return emitter->init();
}

bool
BytecodeCompiler::handleParseFailure(const Directives& newDirectives)
{
    if (parser->hadAbortedSyntaxParse()) {
        // Hit some unrecoverable ambiguity during an inner syntax parse.
        // Syntax parsing has now been disabled in the parser, so retry
        // the parse.
        parser->clearAbortedSyntaxParse();
    } else if (parser->tokenStream.hadError() || directives == newDirectives) {
        return false;
    }

    parser->tokenStream.seek(startPosition);

    // Assignment must be monotonic to prevent reparsing iloops
    MOZ_ASSERT_IF(directives.strict(), newDirectives.strict());
    MOZ_ASSERT_IF(directives.asmJS(), newDirectives.asmJS());
    directives = newDirectives;
    return true;
}

bool
BytecodeCompiler::prepareTree(ParseNode** ppn)
{
    if (!FoldConstants(cx, ppn, parser.ptr()) ||
        !NameFunctions(cx, *ppn))
    {
        return false;
    }

    emitter->setFunctionBodyEndPos((*ppn)->pn_pos);
    return true;
}

bool
BytecodeCompiler::maybeSetDisplayURL(TokenStream& tokenStream)
{
    if (tokenStream.hasDisplayURL()) {
        if (!scriptSource->setDisplayURL(cx, tokenStream.displayURL()))
            return false;
    }
    return true;
}

bool
BytecodeCompiler::maybeSetSourceMap(TokenStream& tokenStream)
{
    if (tokenStream.hasSourceMapURL()) {
        MOZ_ASSERT(!scriptSource->hasSourceMapURL());
        if (!scriptSource->setSourceMapURL(cx, tokenStream.sourceMapURL()))
            return false;
    }
    return true;
}

bool
BytecodeCompiler::maybeSetSourceMapFromOptions()
{
    /*
     * Source map URLs passed as a compile option (usually via a HTTP source map
     * header) override any source map urls passed as comment pragmas.
     */
    if (options.sourceMapURL()) {
        // Warn about the replacement, but use the new one.
        if (scriptSource->hasSourceMapURL()) {
            if(!parser->report(ParseWarning, false, nullptr, JSMSG_ALREADY_HAS_PRAGMA,
                              scriptSource->filename(), "//# sourceMappingURL"))
                return false;
        }

        if (!scriptSource->setSourceMapURL(cx, options.sourceMapURL()))
            return false;
    }

    return true;
}

bool
BytecodeCompiler::deoptimizeArgumentsInEnclosingScripts(JSContext* cx, HandleObject environment)
{
    RootedObject env(cx, environment);
    while (env->is<EnvironmentObject>() || env->is<DebugEnvironmentProxy>()) {
        if (env->is<CallObject>() && !env->as<CallObject>().isForEval()) {
            RootedScript script(cx, env->as<CallObject>().callee().getOrCreateScript(cx));
            if (!script)
                return false;
            if (script->argumentsHasVarBinding()) {
                if (!JSScript::argumentsOptimizationFailed(cx, script))
                    return false;
            }
        }
        env = env->enclosingEnvironment();
    }

    return true;
}

bool
BytecodeCompiler::maybeCompleteCompressSource()
{
    return !maybeSourceCompressor || maybeSourceCompressor->complete();
}

JSScript*
BytecodeCompiler::compileScript(HandleObject environment, SharedContext* sc)
{
    if (!createSourceAndParser())
        return nullptr;

    if (!createScript())
        return nullptr;

    if (!createEmitter(sc))
        return nullptr;

    for (;;) {
        ParseContext pc(parser.ptr(), sc, /* newDirectives = */ nullptr);
        if (!pc.init())
            return nullptr;

        ParseNode* pn;
        if (sc->isEvalContext())
            pn = parser->evalBody();
        else
            pn = parser->globalBody();

        // Successfully parsed. Emit the script.
        if (pn) {
            if (sc->isEvalContext() && sc->hasDebuggerStatement() && cx->isJSContext()) {
                // If the eval'ed script contains any debugger statement, force construction
                // of arguments objects for the caller script and any other scripts it is
                // transitively nested inside. The debugger can access any variable on the
                // scope chain.
                if (!deoptimizeArgumentsInEnclosingScripts(cx->asJSContext(), environment))
                    return nullptr;
            }
            if (!prepareTree(&pn))
                return nullptr;
            if (!emitter->emitScript(pn))
                return nullptr;
            parser->handler.freeTree(pn);

            break;
        }

        // Maybe we aborted a syntax parse. See if we can try again.
        if (!handleParseFailure(directives))
            return nullptr;
    }

    if (!maybeSetDisplayURL(parser->tokenStream) ||
        !maybeSetSourceMap(parser->tokenStream) ||
        !maybeSetSourceMapFromOptions())
    {
        return nullptr;
    }

    if (!maybeCompleteCompressSource())
        return nullptr;

    MOZ_ASSERT_IF(cx->isJSContext(), !cx->asJSContext()->isExceptionPending());

    return script;
}

JSScript*
BytecodeCompiler::compileGlobalScript(ScopeKind scopeKind)
{
    GlobalSharedContext globalsc(cx, scopeKind, directives, options.extraWarningsOption);
    return compileScript(nullptr, &globalsc);
}

JSScript*
BytecodeCompiler::compileEvalScript(HandleObject environment, HandleScope enclosingScope)
{
    EvalSharedContext evalsc(cx, enclosingScope, directives, options.extraWarningsOption);
    return compileScript(environment, &evalsc);
}

ModuleObject*
BytecodeCompiler::compileModule()
{
    if (!createSourceAndParser())
        return nullptr;

    Rooted<ModuleObject*> module(cx, ModuleObject::create(cx));
    if (!module)
        return nullptr;

    if (!createScript())
        return nullptr;

    module->init(script);

    ModuleBuilder builder(cx, module);
    ParseNode* pn = parser->standaloneModule(module, builder);
    if (!pn)
        return nullptr;

    if (!NameFunctions(cx, pn) ||
        !maybeSetDisplayURL(parser->tokenStream) ||
        !maybeSetSourceMap(parser->tokenStream))
    {
        return nullptr;
    }

    RootedModuleEnvironmentObject dynamicScope(cx, ModuleEnvironmentObject::create(cx, module));
    if (!dynamicScope)
        return nullptr;

    module->setInitialEnvironment(dynamicScope);

    if (!createEmitter(pn->pn_modulebox))
        return nullptr;
    if (!emitter->emitModuleScript(pn->pn_body))
        return nullptr;

    if (!builder.initModule())
        return nullptr;

    parser->handler.freeTree(pn);

    if (!maybeCompleteCompressSource())
        return nullptr;

    MOZ_ASSERT_IF(cx->isJSContext(), !cx->asJSContext()->isExceptionPending());
    return module;
}

bool
BytecodeCompiler::compileFunctionBody(MutableHandleFunction fun,
                                      Handle<PropertyNameVector> formals,
                                      GeneratorKind generatorKind)
{
    MOZ_ASSERT(fun);
    MOZ_ASSERT(fun->isTenured());

    fun->setArgCount(formals.length());

    if (!createSourceAndParser())
        return false;

    // Speculatively parse using the default directives implied by the context.
    // If a directive is encountered (e.g., "use strict") that changes how the
    // function should have been parsed, we backup and reparse with the new set
    // of directives.

    ParseNode* fn;
    do {
        Directives newDirectives = directives;
        fn = parser->standaloneFunctionBody(fun, formals, generatorKind, directives,
                                            &newDirectives);
        if (!fn && !handleParseFailure(newDirectives))
            return false;
    } while (!fn);

    if (!NameFunctions(cx, fn) ||
        !maybeSetDisplayURL(parser->tokenStream) ||
        !maybeSetSourceMap(parser->tokenStream))
    {
        return false;
    }

    if (fn->pn_funbox->function()->isInterpreted()) {
        MOZ_ASSERT(fun == fn->pn_funbox->function());

        if (!createScript())
            return false;

        if (!createEmitter(fn->pn_funbox))
            return false;
        if (!emitter->emitFunctionScript(fn->pn_body))
            return false;
    } else {
        fun.set(fn->pn_funbox->function());
        MOZ_ASSERT(IsAsmJSModule(fun));
    }

    if (!maybeCompleteCompressSource())
        return false;

    return true;
}

ScriptSourceObject*
BytecodeCompiler::sourceObjectPtr() const
{
    return sourceObject.get();
}

ScriptSourceObject*
frontend::CreateScriptSourceObject(ExclusiveContext* cx, const ReadOnlyCompileOptions& options)
{
    ScriptSource* ss = cx->new_<ScriptSource>();
    if (!ss)
        return nullptr;
    ScriptSourceHolder ssHolder(ss);

    if (!ss->initFromOptions(cx, options))
        return nullptr;

    RootedScriptSource sso(cx, ScriptSourceObject::create(cx, ss));
    if (!sso)
        return nullptr;

    // Off-thread compilations do all their GC heap allocation, including the
    // SSO, in a temporary compartment. Hence, for the SSO to refer to the
    // gc-heap-allocated values in |options|, it would need cross-compartment
    // wrappers from the temporary compartment to the real compartment --- which
    // would then be inappropriate once we merged the temporary and real
    // compartments.
    //
    // Instead, we put off populating those SSO slots in off-thread compilations
    // until after we've merged compartments.
    if (cx->isJSContext()) {
        if (!ScriptSourceObject::initFromOptions(cx->asJSContext(), sso, options))
            return nullptr;
    }

    return sso;
}

// CompileScript independently returns the ScriptSourceObject (SSO) for the
// compile.  This is used by off-main-thread script compilation (OMT-SC).
//
// OMT-SC cannot initialize the SSO when it is first constructed because the
// SSO is allocated initially in a separate compartment.
//
// After OMT-SC, the separate compartment is merged with the main compartment,
// at which point the JSScripts created become observable by the debugger via
// memory-space scanning.
//
// Whatever happens to the top-level script compilation (even if it fails and
// returns null), we must finish initializing the SSO.  This is because there
// may be valid inner scripts observable by the debugger which reference the
// partially-initialized SSO.
class MOZ_STACK_CLASS AutoInitializeSourceObject
{
    BytecodeCompiler& compiler_;
    ScriptSourceObject** sourceObjectOut_;

  public:
    AutoInitializeSourceObject(BytecodeCompiler& compiler, ScriptSourceObject** sourceObjectOut)
      : compiler_(compiler),
        sourceObjectOut_(sourceObjectOut)
    { }

    ~AutoInitializeSourceObject() {
        if (sourceObjectOut_)
            *sourceObjectOut_ = compiler_.sourceObjectPtr();
    }
};

struct AutoTimer
{
    const char* name;
    timespec start;

    static long elapsedNs(timespec* start, struct timespec* end) {
        return (end->tv_sec - start->tv_sec) * 1000000000 + (end->tv_nsec - start->tv_nsec);
    }

    AutoTimer(const char* name)
      : name(name)
    {
        clock_gettime(CLOCK_MONOTONIC, &start);
    }

    ~AutoTimer() {
        timespec end;
        clock_gettime(CLOCK_MONOTONIC, &end);
        fprintf(stdout, "%s took %ld ns\n", name, elapsedNs(&start, &end));
    }
};

JSScript*
frontend::CompileGlobalScript(ExclusiveContext* cx, LifoAlloc* alloc, ScopeKind scopeKind,
                              const ReadOnlyCompileOptions& options,
                              SourceBufferHolder& srcBuf,
                              SourceCompressionTask* extraSct,
                              ScriptSourceObject** sourceObjectOut)
{
    AutoTimer timer("CompileGlobalScript");

    MOZ_ASSERT(scopeKind == ScopeKind::Global || scopeKind == ScopeKind::NonSyntactic);
    BytecodeCompiler compiler(cx, alloc, options, srcBuf, /* enclosingScope = */ nullptr,
                              TraceLogger_ParserCompileScript);
    AutoInitializeSourceObject autoSSO(compiler, sourceObjectOut);
    compiler.maybeSetSourceCompressor(extraSct);
    return compiler.compileGlobalScript(scopeKind);
}

JSScript*
frontend::CompileEvalScript(ExclusiveContext* cx, LifoAlloc* alloc,
                            HandleObject environment, HandleScope enclosingScope,
                            const ReadOnlyCompileOptions& options,
                            SourceBufferHolder& srcBuf,
                            SourceCompressionTask* extraSct,
                            ScriptSourceObject** sourceObjectOut)
{
    BytecodeCompiler compiler(cx, alloc, options, srcBuf, enclosingScope,
                              TraceLogger_ParserCompileScript);
    AutoInitializeSourceObject autoSSO(compiler, sourceObjectOut);
    compiler.maybeSetSourceCompressor(extraSct);
    return compiler.compileEvalScript(environment, enclosingScope);
}

ModuleObject*
frontend::CompileModule(ExclusiveContext* cx, const ReadOnlyCompileOptions& optionsInput,
                        SourceBufferHolder& srcBuf, LifoAlloc* alloc,
                        ScriptSourceObject** sourceObjectOut /* = nullptr */)
{
    MOZ_ASSERT(srcBuf.get());
    MOZ_ASSERT(alloc);
    MOZ_ASSERT_IF(sourceObjectOut, *sourceObjectOut == nullptr);

    CompileOptions options(cx, optionsInput);
    options.maybeMakeStrictMode(true); // ES6 10.2.1 Module code is always strict mode code.
    options.setIsRunOnce(true);

    BytecodeCompiler compiler(cx, alloc, options, srcBuf, cx->emptyGlobalScope(),
                              TraceLogger_ParserCompileModule);
    AutoInitializeSourceObject autoSSO(compiler, sourceObjectOut);
    return compiler.compileModule();
}

ModuleObject*
frontend::CompileModule(JSContext* cx, const ReadOnlyCompileOptions& options,
                        SourceBufferHolder& srcBuf)
{
    if (!GlobalObject::ensureModulePrototypesCreated(cx, cx->global()))
        return nullptr;

    LifoAlloc* alloc = &cx->asJSContext()->tempLifoAlloc();
    RootedModuleObject module(cx, CompileModule(cx, options, srcBuf, alloc));
    if (!module)
        return nullptr;

    // This happens in GlobalHelperThreadState::finishModuleParseTask() when a
    // module is compiled off main thread.
    if (!ModuleObject::FreezeArrayProperties(cx->asJSContext(), module))
        return nullptr;

    return module;
}

bool
frontend::CompileLazyFunction(JSContext* cx, Handle<LazyScript*> lazy, const char16_t* chars, size_t length)
{
    MOZ_ASSERT(cx->compartment() == lazy->functionNonDelazifying()->compartment());

    CompileOptions options(cx, lazy->version());
    options.setMutedErrors(lazy->mutedErrors())
           .setFileAndLine(lazy->filename(), lazy->lineno())
           .setColumn(lazy->column())
           .setNoScriptRval(false)
           .setSelfHostingMode(false);

    AutoCompilationTraceLogger traceLogger(cx, TraceLogger_ParserCompileLazy, options);

    Parser<FullParseHandler> parser(cx, &cx->tempLifoAlloc(), options, chars, length,
                                    /* foldConstants = */ true, nullptr, lazy);
    if (!parser.checkOptions())
        return false;

    Rooted<JSFunction*> fun(cx, lazy->functionNonDelazifying());
    MOZ_ASSERT(!lazy->isLegacyGenerator());
    ParseNode* pn = parser.standaloneLazyFunction(fun, lazy->strict(), lazy->generatorKind());
    if (!pn)
        return false;

    if (!NameFunctions(cx, pn))
        return false;

    RootedScriptSource sourceObject(cx, lazy->sourceObject());
    MOZ_ASSERT(sourceObject);

    Rooted<JSScript*> script(cx, JSScript::Create(cx, options, sourceObject,
                                                  lazy->begin(), lazy->end()));
    if (!script)
        return false;

    if (lazy->isLikelyConstructorWrapper())
        script->setLikelyConstructorWrapper();
    if (lazy->hasBeenCloned())
        script->setHasBeenCloned();

    BytecodeEmitter bce(/* parent = */ nullptr, &parser, pn->pn_funbox, script, lazy,
                        pn->pn_pos, BytecodeEmitter::LazyFunction);
    if (!bce.init())
        return false;

    return bce.emitFunctionScript(pn->pn_body);
}

// Compile a JS function body, which might appear as the value of an event
// handler attribute in an HTML <INPUT> tag, or in a Function() constructor.
static bool
CompileFunctionBody(JSContext* cx, MutableHandleFunction fun, const ReadOnlyCompileOptions& options,
                    Handle<PropertyNameVector> formals, SourceBufferHolder& srcBuf,
                    HandleScope enclosingScope, GeneratorKind generatorKind)
{
    MOZ_ASSERT(!options.isRunOnce);

    // FIXME: make Function pass in two strings and parse them as arguments and
    // ProgramElements respectively.

    BytecodeCompiler compiler(cx, &cx->tempLifoAlloc(), options, srcBuf, enclosingScope,
                              TraceLogger_ParserCompileFunction);
    compiler.setSourceArgumentsNotIncluded();
    return compiler.compileFunctionBody(fun, formals, generatorKind);
}

bool
frontend::CompileFunctionBody(JSContext* cx, MutableHandleFunction fun,
                              const ReadOnlyCompileOptions& options,
                              Handle<PropertyNameVector> formals, JS::SourceBufferHolder& srcBuf,
                              HandleScope enclosingScope)
{
    return CompileFunctionBody(cx, fun, options, formals, srcBuf, enclosingScope, NotGenerator);
}

bool
frontend::CompileFunctionBody(JSContext* cx, MutableHandleFunction fun,
                              const ReadOnlyCompileOptions& options,
                              Handle<PropertyNameVector> formals, JS::SourceBufferHolder& srcBuf)
{
    return CompileFunctionBody(cx, fun, options, formals, srcBuf, cx->emptyGlobalScope(),
                               NotGenerator);
}


bool
frontend::CompileStarGeneratorBody(JSContext* cx, MutableHandleFunction fun,
                                   const ReadOnlyCompileOptions& options,
                                   Handle<PropertyNameVector> formals,
                                   JS::SourceBufferHolder& srcBuf)
{
    return CompileFunctionBody(cx, fun, options, formals, srcBuf, cx->emptyGlobalScope(),
                               StarGenerator);
}
