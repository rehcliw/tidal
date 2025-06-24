:set -XOverloadedStrings
:set prompt ""

import Sound.Tidal.Context

import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

tidal <- startTidal (superdirtTarget {oLatency = 0.05, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cVerbose = True, cFrameTimespan = 1/20})

:{
let only = (hush >>)
    p = streamReplace tidal
    hush = streamHush tidal
    panic = do hush
               once $ sound "superpanic"
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    unmuteAll = streamUnmuteAll tidal
    unsoloAll = streamUnsoloAll tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setCycle = streamSetCycle tidal
    setcps = asap . cps
    setbpm bpm = setcps (bpm/60/4)
    getcps = streamGetcps tidal
    getnow = streamGetnow tidal
    revOn' vs f = foldr (\x -> inside x rev . rev) f vs
    revOn x = revOn' [x]
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    jumpMod' i t p = transition tidal True (Sound.Tidal.Transition.jumpMod' t p) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

:{
let fmamp op = pF ("amp" ++ show op)
    fmratio op = pF ("ratio" ++ show op)
    fmdt op = pF ("detune" ++ show op)
    fmmod opa opb = pF ("mod" ++ show opa ++ show opb)
    fmegrate op step = pF ("egrate" ++ show op ++ show step)
    fmeglevel op step = pF ("eglevel" ++ show op ++ show step)
    fmfb = pF "feedback"
    lfof = pF "lfofreq"
    lfod = pF "lfodepth" --amplitude 0 - 1
    fmparam function = foldr (#) (gain 1) . zipWith function [1..]
    fma = fmparam fmamp
    fmr = fmparam fmratio
    fmd = fmparam fmdt
    fmer op = fmparam (fmegrate op) -- higher nr = faster
    fmel op = fmparam (fmeglevel op)
    fmm opa = fmparam (fmmod opa)
:}


-- lessDense function https://club.tidalcycles.org/t/limit-events-based-on-delta-time/3121
import Data.List(sortOn)
:{
densityFilter:: Eq a => Double -> [Event a] -> [Event a]
densityFilter density events = foldl (fi density) events [0..length events -1]
                      where fi density es n | length es > n = filter (\e -> e == es!!n || abs ((eventPartStart e) - (eventPartStart (es!!n))) >= toRational density ) es
                                            | otherwise = es

lessDense :: Eq a => Double -> Pattern a -> Pattern a
lessDense density p = p {query = (densityFilter density). sortOn whole . query p}
:}

-- other custom funcs
:{
    let dtfl wet delt delfb lck = ((# delay wet) . (# delaytime delt) . (# delayfeedback delfb) . (# lock lck))
:}

-- tidal-looper
:{
    linput = pI "linput" -- change input bus
    lname = pS "lname" -- change buffer name
:}

:{
let getState = streamGet tidal
    setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}

:script /Users/may/Library/Application\ Support/SuperCollider/synthdefs/mi-ugens-params.hs
:set prompt "tidal> "
:set prompt-cont ""

default (Pattern String, Integer, Double)