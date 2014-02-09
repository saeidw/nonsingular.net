---
title: Picking the right abstraction
author: Saeid
---

I recently found myself writing some C++ code to talk to a service.
The details of the interface between my program and the service were leaking
out all over the code. Worse, those details were tightly coupled to the
library I was using.

This got me thinking about picking the right abstraction for my programs.
I thought surely there must be some way of structuring this thing so that
the details aren't all over the place.

I settled for a couple of objects exposing the functionality I needed
and handling the ugly details of speaking to the library. To the rest of my
program, communicating with the outside world is simply an act of instantiating
one of these objects, calling its methods and handling any exceptions that
come up.

Hopefully this abstraction will hold, but you know how in real life things
tend to be more complicated. I think I'll be finding out soon how well these
assumptions fit in with the runtime environment.

