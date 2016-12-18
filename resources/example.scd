// Realtime playback of a musical score.
// Run like this: <path-to-sclang> <path-to-this-file> <path-to-score-file>

Server.default.waitForBoot({

  {
    SynthDef(\sine, {
      // Example call: Synth(\sine, [\frequency, 80, \duration, 8, \amplitude, 0.8]);
      arg frequency = 220,
      duration  = 1,
      amplitude = 0.5,
      out       = 0;

      var source = SinOsc.ar(frequency);
      var env    = EnvGen.kr(Env.triangle(dur:duration, level:1), doneAction:2);

      Out.ar(out, Splay.ar(source * env * amplitude));
    }).add;

    // Wait for SynthDef to reach server
    s.sync;

    // Play score
    Score.newFromFile(thisProcess.argv[0]).play;

  }.fork;

});