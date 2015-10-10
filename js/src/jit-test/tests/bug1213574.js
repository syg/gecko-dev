var lfGlobal = newGlobal();
lfGlobal.offThreadCompileScript(`let (x) { throw 42; }`);
try {
    lfGlobal.runOffThreadScript();
} catch (e) {
}
