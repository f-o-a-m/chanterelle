.. _deployments:

===========
Deployments
===========

Configuration
-------------

Every contract deployment requires an explicit configuration. Specifically, the configuration is and object of the following type:

.. code-block:: haskell

   type ContractConfig args =
       { filepath :: String
       , name :: String
       , constructor :: Constructor args
       , unvalidatedArgs :: V (Array String) (Record args)
       }

The ``filepath`` field is the filepath to the solc build artifact relative the the ``chanterelle.json`` file.

The ``name`` field is there to name the deployment throughout the logging. (This could dissappear assuming its suffient to name the deployment according to the build artifact filename.)

The type ``Constructor args`` is a type synonym:

.. code-block:: haskell

   type Constructor args = forall eff. TransactionOptions NoPay -> HexString -> Record args -> Web3 eff HexString

In other words, ``Constructor args`` is the type a function taking in some ``TransactionOptions NoPay`` (constructors are not payable transactions), the deployment bytecode, and a record of type ``Record args``. It will format the transaction and submit it via an ``eth_sendTransaction`` RPC call, returning the transaction hash as a ``HexString``.

The ``unvalidatedArgs`` field has type ``V (Array String) (Record args)`` where ``V`` is a type coming from the `purescript-validation <https://github.com/purescript/purescript-validation>`_ library. This effectively represents `either` a type of ``Record args`` `or` a list of error messages for all arguments which failed to validate.

It's possible that your contract requires no arguments for deployment, and in that case chanterelle offers some default values. For example, if the filepath of the build artifact for ``VerySimpleContract.sol`` is ``build/VerySimpleContract.json``, you might end up with something like

.. code-block:: haskell

   verySimpleContractConfig :: ContractConfig NoArgs
   verySimpleContractConfig =
     { filepath: "build/VerySimpleContract.json"
     , name: "VerySimpleContract"
     , constructor: constructorNoArgs
     , unvalidatedArgs: noArgs
     }

Let's consider the simplest example of a contract configuration requiring a constructor with arguments. Consider the following smart contract:

.. code-block:: js

   contract SimpleStorage {

     uint256 count public;
      
     function SimpleStorage(uint256 initialCount) {
       count = initialCount;
     }

     ...

   }

Depending on your project configuration, when running ``chanterelle compile`` you should end up with something like the following artifacts:

1. The solc artifact ``build/SimpleStorage.json``
2. The generated PureScript file ``src/Contracts/SimpleStorage.purs``

In the PureScript module ``Contracts.SimpleStorage``, you will find a function

.. code-block:: haskell

   constructor :: forall e. TransactionOptions NoPay -> HexString -> {initialCount :: UIntN (D2 :& D5 :& DOne D6)} -> Web3 e HexString

Blurring your eyes a little bit, it's easy to see that this indeed matches up to the constructor defined in the Solidity file. We could then define the deployment configuration for ``SimpleStorage`` as

.. code-block:: haskell

   import Contracts.SimpleStorage as SimpleStorage

   simpleStorageConfig :: ContractConfig (initialCount :: UIntN (D2 :& D5 :& DOne D6))
   simpleStorageConfig =
       { filepath: "build/SimpleStorage.json"
       , name: "SimpleStorage"
       , constructor: SimpleStorage.constructor
       , unvalidatedArgs: validCount
       }
     where
       validCount = uIntNFromBigNumber s256 (embed 1234) ?? "SimpleStorage: initialCount must be valid uint256"

Here you can see where validation is important. Clearly ``1234`` represents a valid ``uint``, but you can easily imagine scenarios where this might save us a lot of trouble-- too many characters in an address, an improperly formatted string, an integer is out of a bounds, etc.
