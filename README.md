# ``finitary-derive``

## What's this all about, then?

Have you ever written an ``Unbox`` instance for a user-defined type? I hope not,
because it's a [uniquely tedious chore][1]. If your type is more complex, this
can be difficult, fiddly, and frustrating. ``Storable`` is not much better. This
is the kind of 'work' that we as Haskellers ought not to put up with.

Now, you don't have to! As long as your type is [``Finitary``][2], you can now
get ``Unbox`` and ``Storable`` (as well as ``Binary`` and ``Hashable``, because 
we could) instances _almost_ automagically:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

import Data.Finitary
import Data.Finitary.Pack
import Data.Word
import Data.Hashable

import qualified Data.Vector.Unboxed as VU

data Foo = Bar | Baz (Word8, Word8) | Quux Word16
  deriving (Eq, Generic, Finitary)
  deriving (Storable, Binary, Hashable) via (Pack Foo)

someVector :: VU.Vector (Pack Foo)
someVector = VU.fromList . fmap Pack $ [Bar, Baz 0x0 0xf, Quux 0x134]
```

If you don't have access to ``DerivingVia``, you can still get the benefits of
this library -- just use ``Pack a`` instead of ``a`` in all cases where you need
any such instances. As it is a ``newtype``, you can ``coerce`` through it if you
care about efficiency.

## Why can't I automagic up ``Unbox`` too?

The short answer is 'role restrictions on unboxed vectors'. If you want a more
detailed explanation, check out the [GHC wiki on roles][3], as well as the
[implementation of ``Data.Vector.Unboxed``][4]. You might also want to check out
[stuff about data families][5], as it ties into this rather aggravating
limitation closely too.

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
