/* Any copyright is dedicated to the Public Domain.
   http://creativecommons.org/publicdomain/zero/1.0/ */

/**
 * Tests that the JIT Optimizations view renders optimization data
 * if on, and displays selected frames on focus.
 */
function spawnTest () {
  let { panel } = yield initPerformance(SIMPLE_URL);
  let {
    EVENTS, $, $$, window, PerformanceController, DetailsView, JITOptimizationsView, JsCallTreeView, RecordingsView
  } = panel.panelWin;

  let profilerData = { threads: [{samples: gSamples, optimizations: gOpts}] };

  is(Services.prefs.getBoolPref(JIT_PREF), false, "show JIT Optimizations pref off by default");

  // Make two recordings, so we have one to switch to later, as the
  // second one will have fake sample data
  yield startRecording(panel);
  yield stopRecording(panel);

  yield startRecording(panel);
  yield stopRecording(panel);
  yield DetailsView.selectView("js-calltree");
  is($("#jit-optimizations-view").hidden, true, "JIT Optimizations panel is hidden when pref off.");

  let thread = JsCallTreeView._prepareCallTree(profilerData, { startTime: 0, endTime: 20 }, {});
  JsCallTreeView._populateCallTree(thread, {});
  is($("#jit-optimizations-view").hidden, true, "JIT Optimizations panel still hidden when rerendered");

  Services.prefs.setBoolPref(JIT_PREF, true);
  is($("#jit-optimizations-view").hidden, false, "JIT Optimizations should be visible when pref is on");

  ok($("#jit-optimizations-view").classList.contains("empty"),
    "JIT Optimizations view has empty message when no frames selected.");

  Services.prefs.setBoolPref(JIT_PREF, false);

  // Click the "A" frame
  let rendered = once(JITOptimizationsView, EVENTS.OPTIMIZATIONS_RENDERED);
  mousedown(window, $$(".call-tree-item")[1]);
  Services.prefs.setBoolPref(JIT_PREF, true);
  yield rendered;
  ok(true, "JITOptimizationsView rendered when enabling with the current frame node selected");

  let id = 0;
  ok($(`.tree-widget-container li[data-id='["${id}","${id}-types","${id}-types-0"]']`),
    "found an ion type row");
  is($$(`.tree-widget-container li[data-id='["${id}","${id}-attempts"]'] .tree-widget-children .tree-widget-item`).length, 3,
    "found 3 attempts");
  id = 1;
  ok(!$(`.tree-widget-container li[data-id='["${id}","${id}-types","${id}-types-0"]']`),
    "only one optimization site");
  ok(!$("#jit-optimizations-view").classList.contains("empty"),
    "JIT Optimizations view has no empty message.");
  ok(!$(`.tree-widget-container .opt-icon[severity=warning]`),
    "did not find a warning icon for no successful strategies");

  // Click the "B" frame
  rendered = once(JITOptimizationsView, EVENTS.OPTIMIZATIONS_RENDERED);
  mousedown(window, $$(".call-tree-item")[2]);
  yield rendered;
  ok(true, "JITOptimizationsView rendered on frame select");

  ok($(`.tree-widget-container li[data-id='["${id}","${id}-types","${id}-types-0"]']`),
    "found an ion type row");
  is($$(`.tree-widget-container li[data-id='["${id}","${id}-attempts"]'] .tree-widget-children .tree-widget-item`).length, 3,
    "found 3 attempts");
  ok($(`.tree-widget-container .opt-icon[severity=warning]`),
    "found a warning icon for no successful strategies");

  // Click the "C" frame
  let reset = once(JITOptimizationsView, EVENTS.OPTIMIZATIONS_RESET);
  mousedown(window, $$(".call-tree-item")[3]);
  yield reset;
  ok(true, "JITOptimizations view reset when clicking a frame with no opt data.");
  ok($("#jit-optimizations-view").classList.contains("empty"),
    "JIT Optimizations view has an empty message when selecting a frame without opt data.");

  let select = once(PerformanceController, EVENTS.RECORDING_SELECTED);
  reset = once(JITOptimizationsView, EVENTS.OPTIMIZATIONS_RESET);
  RecordingsView.selectedIndex = 0;
  yield Promise.all([select, reset]);
  ok(true, "JITOptimizations view correctly reset when switching recordings.");

  yield teardown(panel);
  finish();
}

let gSamples = [{
  time: 5,
  frames: [
    { category: 8,  location: "(root)" },
    { category: 8,  location: "A (http://foo/bar/baz:12)", optsIndex: 0 },
    { category: 16, location: "B (http://foo/bar/baz:34)", optsIndex: 1 },
    { category: 32, location: "C (http://foo/bar/baz:56)" }
  ]
}, {
  time: 5 + 1,
  frames: [
    { category: 8,  location: "(root)" },
    { category: 8,  location: "A (http://foo/bar/baz:12)" },
    { category: 16, location: "B (http://foo/bar/baz:34)" },
    { category: 64, location: "D (http://foo/bar/baz:78)" }
  ]
}, {
  time: 5 + 1 + 2,
  frames: [
    { category: 8,  location: "(root)" },
    { category: 8,  location: "A (http://foo/bar/baz:12)", optsIndex: 0 },
    { category: 16, location: "B (http://foo/bar/baz:34)" },
    { category: 64, location: "D (http://foo/bar/baz:78)" }
  ]
}, {
  time: 5 + 1 + 2 + 7,
  frames: [
    { category: 8,   location: "(root)" },
    { category: 8,   location: "A (http://foo/bar/baz:12)", optsIndex: 0 },
    { category: 128, location: "E (http://foo/bar/baz:90)" },
    { category: 256, location: "F (http://foo/bar/baz:99)" }
  ]
}];

// Array of OptimizationSites
let gOpts = [{
  line: 12,
  column: 2,
  types: [{ mirType: "Object", site: "A (http://foo/bar/bar:12)", types: [
    { keyedBy: "constructor", name: "Foo", location: "A (http://foo/bar/baz:12)" },
    { keyedBy: "primitive", location: "self-hosted" }
  ]}],
  attempts: [
    { outcome: "Failure1", strategy: "SomeGetter1" },
    { outcome: "Failure2", strategy: "SomeGetter2" },
    { outcome: "Inlined", strategy: "SomeGetter3" },
  ]
}, {
  line: 34,
  types: [{ mirType: "Int32", site: "Receiver" }], // use no types
  attempts: [
    { outcome: "Failure1", strategy: "SomeGetter1" },
    { outcome: "Failure2", strategy: "SomeGetter2" },
    { outcome: "Failure3", strategy: "SomeGetter3" },
  ]
}];
