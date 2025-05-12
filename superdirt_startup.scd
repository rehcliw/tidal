(
s.reboot { // server options are only updated on reboot
	// configure the sound server: here you could add hardware specific options
	// see http://doc.sccode.org/Classes/ServerOptions.html
	s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
	s.options.memSize = 8192 * 32; // increase this if you get "alloc failed" messages
	s.options.numWireBufs = 2048; // increase this if you get "exceeded number of interconnect buffers" messages
	s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
	s.options.numOutputBusChannels = 2; // set this to your hardware output channel size, if necessary
	s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
	s.options.inDevice_("USB Audio CODEC "); // for tidal looping w external input (xenyx)
	// s.options.inDevice_("Volt 2"); // for tidal looping w volt 2
	// s.options.inDevice_("MiniFuse 2 Start");
	// s.options.inDevice_("Aggregate Device"); // for tidal looping self t
	s.options.outDevice_("Screen Recording Audio"); // for screen recording
	// s.options.outDevice_("MacBook Pro Speakers");
	// s.options.outDevice_("WH-1000XM4");
	// s.options.outDevice_("External Headphones");
	// s.options.outDevice_("24E3");
	// boot the server and start SuperDirt
	s.waitForBoot {
		~dirt.stop; // stop any old ones, avoid duplicate dirt (if it is nil, this won't do anything)
		~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
		
		// Initialize the TidalLooper
		~looper = TidalLooper(~dirt);

		// You can adjust these parameter even in runtime
		~looper.rLevel = 1.5; // recording level
		~looper.pLevel = 1; // prerecording level
		// ~looper.linput = 0; // Default 0; Set this to your main input port.
		// ~looper.lname = "loop"; // default "loop"

		// load mi-ugens.scd synthdefs
		load("Users/may/Library/Application Support/SuperCollider/synthdefs/mi-ugens.scd");

		// load custom synthdefs
		load("/Users/may/Documents/music-tools/tidal/synths.scd");

		~dirt.loadSoundFiles; // load samples (path containing a wildcard can be passed in)
		// for example: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
		~dirt.loadSoundFiles("/Users/may/Documents/music-tools/Samples/load/*");
		~dirt.loadSoundFiles("/Users/may/Documents/music-tools/Samples/load/*/*");

		// s.sync; // optionally: wait for samples to be read
		~dirt.start(57120, 0 ! 12); // start listening on port 57120, create two busses each sending audio to channel 0
		SuperDirt.default = ~dirt; // make this instance available in sclang (optional)

		// optional, needed for convenient access from sclang:
		(
			~d1 = ~dirt.orbits[0]; ~d2 = ~dirt.orbits[1]; ~d3 = ~dirt.orbits[2];
			~d4 = ~dirt.orbits[3]; ~d5 = ~dirt.orbits[4]; ~d6 = ~dirt.orbits[5];
			~d7 = ~dirt.orbits[6]; ~d8 = ~dirt.orbits[7]; ~d9 = ~dirt.orbits[8];
			~d10 = ~dirt.orbits[9]; ~d11 = ~dirt.orbits[10]; ~d12 = ~dirt.orbits[11];
		);

		// directly below here, in your own copy of this file, you could add further code that you want to call on startup
		// this makes sure the server and ~dirt are running
		// you can keep this separate and make it easier to switch between setups
		// by using "path/to/my/file.scd".load and if necessary commenting out different load statements
		// ...

		// define global effects for mutable instruments effects
		~dirt.orbits.do { |x|
			var clouds = GlobalDirtEffect(\global_mi_clouds, [\cloudspitch, \cloudspos, \cloudssize, \cloudsdens, \cloudstex, \cloudswet, \cloudsgain, \cloudsspread, \cloudsrvb, \cloudsfb, \cloudsfreeze, \cloudsmode, \cloudslofi]);
			var verb = GlobalDirtEffect(\global_mi_verb, [\verbwet, \verbtime, \verbdamp, \verbhp, \verbfreeze, \verbdiff, \verbgain]);
			x.globalEffects = x.globalEffects
				.addFirst(clouds)
				.addFirst(verb); 
			x.initNodeTree;    
		};                     
		// end define global effects for mutable instruments effects

	};

	s.latency = 0.3; // increase this if you get "late" messages

};
);
