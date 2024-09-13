(
// borrowed from 20240522-foldtest.scd
// should refactor to have take in a freq arg, so tidal can use n argument.. but I do like having a freq envelope. maybe it would respond to accelerate? eh let's see how it sounds!
SynthDef(\foldy, {
	arg out=0, freq=200, fend=200, dur, freqfoldhi=500, sigfoldhi=1, mstart=2, mend=2, mastart=2, maend=2, astart=0.5, aend=32, rel=1, atk=0.01;
	var sig, moda, amp, mfenv, env;

	// fend = freq;

	mfenv = EnvGen.kr(Env.new(
		[
			{mstart * Rand(0.9, 1.1)}!4,
			{mend * Rand(0.9, 1.1)}!4
		],
		[dur * rel],
		\lin)
	);
	freq = EnvGen.kr(Env.new(
		[
			{freq * Rand(0.9, 1.1)}!4,
			{freq * Rand(0.9, 1.1)}!4
		],
		[dur * rel],
		\exp)
	) * mfenv;
	freq = Fold.kr(freq, 20, freqfoldhi);

	sig = VarSaw.ar(freq);

	moda = EnvGen.kr(Env.new(
		[
			{mastart * Rand(0.9, 1.1)}!4,
			{maend * Rand(0.9, 1.1)}!4
		],
		[dur * rel],
		\lin));
	amp = EnvGen.kr(Env.new(
		[
			{astart * Rand(0.9, 1.1)}!4,
			{aend * Rand(0.9, 1.1)}!4
		],
		[dur * rel],
		\exp)
	) * moda;
	sig = sig * amp;
	sig = Clip.ar(sig, sigfoldhi.neg, sigfoldhi);

	env = EnvGen.kr(Env.new(
		[0, 1, 0.75, 0],
		[
			dur * rel * atk,
			dur * rel * 0.09,
			dur * rel * rel
		],
		[1, 1, -1]
	), doneAction: 2);
	sig = sig * env;

	sig = Splay.ar(sig, center: LFNoise1.kr(0.01));
	Out.ar(out, sig)
}).add;

)