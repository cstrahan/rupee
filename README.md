# Rupee

Rupee is a Haskell package that makes it easy to to embed and extend
Ruby in Haskell.

This is currently a work in progress, but the features do seem to be
filling in fairly rapidly.

# What makes it special

This package makes every effort to ensure a safe coding experience.
To that end, exceptions are properly marshalled in both directions, and
GC eligibility is carefully thought out. Implementing any of these
features in an ad-hoc fashion is bound to be a frustrating experience,
fraught with error; if you take a peek at the implementation, you'll see
what sort of hoops that must be jumped through to provide a robust
binding.
