.. _modules:

=======
Modules
=======

Chanterelle uses a notion of modules to determine which units of Solidity code to compile and create PureScript bindings for. Those coming from a Haskell background may notice the parallel to Cabal's exposed-modules. Simply adding a contract file within the source-dir does not mean it will be compiled, nor will there be a generate PureScript file. Instead, one must explicitly specify it in the project's :ref:`chanterelle-json`.

Chanterelle uses module namespacing for Solidity files similar to what's found in PureScript or Haskell, though here we enforce that the module name must mirror the filepath. A module named ``Some.Modular.Contract`` is expected to be in ``contracts/Some/Modular/Contract.sol``, and its PureScript binding will have the same module name as well.

Solidity build artifacts are put into the ``build/`` directory by default (see ``artifacts-dir`` in :ref:`chanterelle-json`). The full artifact filepath corresponds to the its relative location in the ``source-dir``. For example, in the `parking-dao <https://github.com/f-o-a-m/parking-dao>`_ the ``ParkingAuthority`` Solidity module is located at ``contracts/ParkingAuthority.sol``. Thus the build artifact will be written to ``build/ParkingAuthority.json``.

If a module-prefix is specified in the purescript-generator section, that module prefix will be prepended to the generated PureScript modules' names. In the parking-dao example, the module ``FoamCSR`` is expected to be located in ``contracts/FoamCSR.sol`` and will generate a PureScript module ``Contracts.FoamCSR`` with filepath ``src/Contracts/FoamCSR.purs``. Likewise, ``Nested.SimpleStorage`` is expected to be located in ``contracts/Nested/SimpleStorage.sol`` and the generated PureScript module name will be ``Contracts.Nested.SimpleStorage`` with filepath ``src/Contracts/Nested/SimpleStorage.purs``.
