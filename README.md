# ``finitary-derive``

## What's this all about, then?

Have you ever written an ``Unbox`` instance for a user-defined type? I hope not,
because it's a [uniquely tedious chore][1]. If your type is more complex, this
can be difficult, fiddly, and frustrating. ``Storable`` is not much better. This
is the kind of 'work' that we as Haskellers ought not to put up with.

Now, you don't have to! As long as your type is [``Finitary``][2], you can now
get ``Unbox`` and ``Storable`` (as well as a whole bunch of other) instances 
_almost_ automagically:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

import Data.Finitary
import Data.Finitary.Finiteness
import Data.Finitary.PackInto
import Data.Word
import Data.Hashable

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

data Foo = Bar | Baz (Word8, Word8) | Quux Word16
  deriving (Eq, Generic, Finitary)
  deriving (Ord, Bounded, Hashable, NFData, Binary) via Finiteness

someVector :: VU.Vector (PackInto Foo Word64)
someVector = VU.fromList . fmap Packed $ [Bar, Baz 0x0 0xf, Quux 0x134]

someStorableVector :: VS.Vector (PackInto Foo Word64)
someStorableVector = VS.fromList . fmap Packed $ [Bar, Baz 0x0 0xf, Quux 0x134]
```

If you don't have access to ``DerivingVia``, you can still get the benefits of
this library -- just use ``Finitary a`` instead of ``a``. As it is a ``newtype``, 
you can ``coerce`` through it if you care about efficiency.

## What's the deal with ``Unbox`` and ``Storable`` exactly? What's with all the ``Pack`` types?

Essentially, being ``Finitary`` means that there's a finite set of indexes, one
for each inhabitant. That means we can essentially represent any inhabitant as a
fixed-length number. It's on the basis of this that we can 'magic up'
``Storable`` and ``Unbox``.

However, how we _represent_ this fixed-length number isn't immediately obvious.
We have a couple of options:

- A string of bits
- A string of bytes
- An array of machine words

Additionally, if we have _another_ finitary type whose cardinality is not
smaller, we could potentially 'borrow' its instances as well. Which of these
choices is appropriate isn't obvious in general: it depends on whether you care
about space or speed, the cardinality of the type, and a bunch of other things
too. As we believe that the best people to judge tradeoffs like these are the
people using our library, we provide _all_ of these options for you to choose
from, so that you can choose the one that best suits you.

## So... what's the difference exactly?

``PackBits`` represents indexes as strings of bits - most compact, but least
efficient speed-wise. ``PackBytes`` represents indexes as byte strings - faster,
but less compact, especially if your type isn't big. ``PackWords`` represents
indexes as fixed-length arrays of ``Word``s - fastest, but not space-efficient
unless your type is really large (like, several multiples of @Word@ large).
Lastly, ``PackInto`` lets you choose another finitary type whose instances you
want to 'borrow' - most flexibility, but requires you to choose an appropriate
type with a cardinality no smaller than yours.

## Why can't I ``DerivingVia`` through these ``Pack`` types?

For ``Unbox``, the short answer is 'role restrictions on unboxed vectors'. If
you want a more detailed explanation, check out the [GHC wiki on roles][3], as
well as the [implementation of ``Data.Vector.Unboxed``][4]. You might also want
to check out [stuff about data families][5]. 

Additionally, there is some tension in the design. We could have made one of two
choices: either define ``Pack`` types as transparent ``newtype``s, and encode or
decode whenever a type class method required it; or define ``Pack`` types as
opaque, and encode or decode only when the values were constructed or
deconstructed. Ultimately, we went with the second option, as it makes the
occurences of encodes and decodes explicit to the user. Had we gone with the
first choice, it would be unclear where encodes and decodes occur, especially
when using functions built from type class methods. We believe this clarity is
worth the inability to use @DerivingVia@ to define ``Storable`` instances.

## Why do ``PackBytes``, ``PackWords`` and ``PackInto`` have ``Storable``
instances, but not ``PackBits``?

Because it's not clear what they should be. Let's suppose you want to bit-pack a
type ``Giraffe`` with cardinality 11 - what should ``sizeOf`` for @PackBits
Giraffe@ be? How about ``alignment``? The only obvious solution is padding, but
in this case, you might as well use ``PackBytes``, ``PackWords`` or
``PackInto``, since then you'll at least know what you're getting, and would be
explicit about it.

## Sounds good! Can I use it?

Certainly - we've tested on GHC 8.4.4, 8.6.5 and 8.8.1, on GNU/Linux only. If
you would like support for any additional GHC versions, let us know.
Unfortunately, while the library will _build_ on 8.4.4, due to
``hedgehog-classes`` being limited to 8.6+, tests cannot be run on this version.

If you build and use this library successfully on any other platforms, we'd like
to know too - it'd be beneficial even if nothing breaks, and _especially_ if
something does.

## License

This library is under the GNU General Public License, version 3 or later (SPDX
code ``GPL-3.0-or-later``). For more details, see the ``LICENSE.md`` file.

[1]: http://hackage.haskell.org/package/vector-0.12.0.3/docs/Data-Vector-Unboxed.html
[2]: https://hackage.haskell.org/package/finitary-1.0.0.1/docs/Data-Finitary.html#t:Finitary
[3]: https://gitlab.haskell.org/ghc/ghc/wikis/roles
[4]: http://hackage.haskell.org/package/vector-0.12.0.3/docs/src/Data.Vector.Unboxed.Base.html
[5]: https://wiki.haskell.org/GHC/Type_families
