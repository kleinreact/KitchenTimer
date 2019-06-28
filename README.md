# TSL Kitchen Timer

We use [Temproal Stream Logic
(TSL)](https://www.react.uni-saarland.de/publications/FKPS19a.html)
for the specification for a real-world kitchen timer application.
First, a Control Flow Model (CFM) is synthesized from the
specification, which then is specialized to a desktop application
using the Arrowized FRP library
[Yampa](https://wiki.haskell.org/Yampa), a web application using the
Monadic [Threepenny-GUI](https://wiki.haskell.org/Threepenny-gui)
library, and to hardware using the Applicative hardware description
language [CλaSH](https://clash-lang.org/).

More details on how the application works and how code is generated
from the CFM can be found
[here](https://www.react.uni-saarland.de/publications/FKPS19b.html).

# The Kitchen Timer Specification

The specification uses
[(TSL)](https://www.react.uni-saarland.de/publications/FKPS19a.html)
for realizing the following requirements:

1. Whenever the `MIN` and `SEC` buttons are pressed simultaneously,
  the timer is reset, meaning the time is set to zero and the system
  stays idle until the next button gets pressed.
2. If only the `MIN` button is pressed and the timer is not currently
  counting up or down, then the currently set time is increased by one
  minute.
3. If only the `SEC` button is pressed and the timer is not currently
  counting up or down, then the currently set time is increased by one
  second.
4. As long as no time greater than zero has been set and the system is
  idle: if the `START/STOP` button is pressed and the timer is not
  already counting up or down, then the timer starts counting up until
  it is stopped by any button pressed.
5. If a time has been set and the `START/STOP` button is pressed while
  the timer is not currently counting up or down, then the timer
  starts counting down until it is stopped by any button pressed.
6. The timer can only be started by pressing start.
7. The timer can always be stopped by pressing any button while
  counting up or down.
8. It is possible to start the timer and to set some time
  simultaneously.
9. The buzzer beeps on any button press and after the counter
  reaches zero while counting down.
10. The display always shows the time that is currently set.

# Requirements

The desktop and web applications run on the most resent OS
distributions. They should work for Linux, iOS, and Windows. For
playing with the hardware variant of the timer, the respective
hardware must be purchased and some adapters must be created. More
details on the individual steps are provided [here (TBD)]().

# Installation

First, create a copy of `build.cfg_sample` and safe it to `build.cfg`:

`cp build.cfg_sample build.cfg`

The control flow model is synthesized by using `make` in the top-level
directory. The applications are build similarly using `make` in the
respective sub-directories. For the synthesis and building processes
to run smoothly, the following tools must be installed and the
respective paths must be set in `build.cfg`.

## Synthesis

* [tsltools](https://github.com/reactive-systems/tsltools) (provides
  `tsl2tlsf`, `cfm2code`)
* An LTL synthesizer that adheres to the rules of the LTL Track of the
  [Reactive Synthesis Competition
  (SYNTCOMP)](http://www.syntcomp.org/). We used [Strix
  (v18.07)](https://strix.model.in.tum.de/), since it synthesizes a
  CFM from the TSL specification within a few seconds. However, other
  alternatives also are for example
  [BoSy](https://github.com/reactive-systems/bosy), ore
  [PARTY](https://github.com/5nizza/party-elli).
  
## Desktop + Web Application

We recommend using the [Haskell Tool Stack](http://haskellstack.org/)
for building both applications. The tool automatically pulls the
correct version of the Glasgow Haskell Compiler (GHC) and all required
dependencies, whilet their installation will not interfer with any
system installation.

## Hardware Application

* [CλaSH compiler](https://github.com/clash-lang/clash-compiler) (commit fff4606)
* [Yosys](https://github.com/cliffordwolf/yosys)
* [nextpnr](https://github.com/YosysHQ/nextpnr)
* [icepack](https://github.com/cliffordwolf/icestorm/tree/master/icepack)
* [iceprog](https://github.com/cliffordwolf/icestorm/tree/master/iceprog)
* [icedude](https://github.com/reactive-systems/icedude)


