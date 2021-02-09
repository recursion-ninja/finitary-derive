# Revision history for finitary-derive

## 2.2.0.1 -- 2021-02-09

* Update bounds for compatibility with GHC 9.0

## 2.2.0.0 -- 2019-11-27

* Fix definition of ``Packed`` for ``PackInto`` to actually agree with the
  documentation.

## 2.1.0.0 -- 2019-11-24

* Fix bug in ``Ord`` instances for the ``Pack*`` types.
* Fix definition of ``Packed`` pattern to actually agree with the documentation.
* Define a ``newtype`` wrapper for better provision of ``Binary`` and
  ``Hashable`` instances for ``Vector``s of ``PackBits`` types.
* Remove ``Hashable`` and ``Binary`` instances for ``PackBits`` (both
  varieties).
* Fix documentation typoes.

## 2.0.0.0 -- 2019-11-23

* Remove ``Data.Finitary.Pack``.
* Add ``Data.Finitary.PackBits``, ``Data.Finitary.PackWords``,
  ``Data.Finitary.PackBytes``, ``Data.Finitary.PackBits.Unsafe`` and
  ``Data.Finitary.PackInto``
* Refactor 'packing-agnostic' functionality into ``Data.Finitary.Finiteness``.
* A lot of documentation changes.

## 1.0.0.1 -- 2019-09-21

* Fix documentation.
* Raise bounds on ``finitary`` to avoid critical bugs.
* Raise bounds on ``base`` for test (for honesty reasons).

## 1.0.0.0 -- 2019-09-17

* First version. Released on an unsuspecting world.
