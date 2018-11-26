.. _deployments:

===========
Deployments
===========

Configuration
-------------

Every contract deployment requires an explicit configuration. Specifically, the configuration is an object of the following type:

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

   type Constructor args = forall eff. TransactionOptions NoPay -> HexString -> Record args -> Web3 HexString

In other words, ``Constructor args`` is the type a function taking in some ``TransactionOptions NoPay`` (constructors are not payable transactions), the deployment bytepurescriptcode, and a record of type ``Record args``. It will format the transaction and submit it via an ``eth_sendTransaction`` RPC call, returning the transaction hash as a ``HexString``.

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

     event CountSet(uint256 _count);
    
     function SimpleStorage(uint256 initialCount) {
       count = initialCount;
     }

     function setCount(uint256 newCount)) {
       count = newCount;
       emit CountSet(newCount));
     }

   }

Depending on your project configuration, when running ``chanterelle compile`` you should end up with something like the following artifacts:

1. The solc artifact ``build/SimpleStorage.json``
2. The generated PureScript file ``src/Contracts/SimpleStorage.purs``

In the PureScript module ``Contracts.SimpleStorage``, you will find a function

.. code-block:: haskell

   constructor :: TransactionOptions NoPay -> HexString -> {initialCount :: UIntN (D2 :& D5 :& DOne D6)} -> Web3 HexString

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


Deploy Scripts
--------------

Deploy scripts are written inside the ``DeployM`` monad, which is a monad that gives you access to a web3 connection, controlled error handling, and whatever effects you want. The primary workhorse is the ``deployContract`` function:

.. code-block:: haskell

   deployContract :: TransactionOptions NoPay -> ContractConfig args -> DeployM {deployAddress :: Address, deployArgs :: Record args}

This function takes your contract deployment configuration as defined above and sends the transaction. If no errors are thrown, it will return the address where the contract as deployed as well as the deploy arguments that were validated before the transaction was sent. It will also automatically write to the solc artifact in the ``artifacts-dir``, updating the ``networks`` object with a key value pair mapping the networkId to the deployed address.

Error hanlding is built in to the ``DeployM`` monad. Unless you want to customize your deployment with any attempt to use some variant of try/catch, any error encountered before or after a contract deployment will safely terminate the script and you should get an informative message in the logs. It will not terminate while waiting for transactions to go through unless the timeout threshold is reached. You can configure the duration as a command line argument.

Deployment Example
------------------

Consider this example take from the parking-dao example project:

.. code-block:: haskell


   module MyDeployScript where

   import ContractConfig (simpleStorageConfig, foamCSRConfig, parkingAuthorityConfig)

   type DeployResults = (foamCSR :: Address, simpleStorage :: Address, parkingAuthority :: Address)

   deployScript :: forall eff. DeployM (Record DeployResults)
   deployScript = do
     deployCfg@(DeployConfig {primaryAccount}) <- ask
     let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
         txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                            # _gas ?~ bigGasLimit
     simpleStorage <- deployContract txOpts simpleStorageConfig
     foamCSR <- deployContract txOpts foamCSRConfig
     let parkingAuthorityConfig = makeParkingAuthorityConfig {foamCSR: foamCSR.deployAddress}
     parkingAuthority <- deployContract txOpts parkingAuthorityConfig
     pure { foamCSR: foamCSR.deployAddress
          , simpleStorage: simpleStorage.deployAddress
          , parkingAuthority: parkingAuthority.deployAddress
          }

After setting up the ``TransactionOptions``, the script first deploys the ``SimpleStorage`` contract and then the ``FoamCSR`` contract using their configuration. The ``ParkingAuthority`` contract requires the address of the ``FoamCSR`` contract as one of it's deployment arguments, so you can see us threading it in before deploying. Finally, we simple return all the addresses of the recently deployed contracts to the caller.

Note that if we simply wanted to terminate the deployment script after the contract deployments there then there's no point in returning anything at all. However, deployment scripts are useful outside of the context of a standalone script. For example you can run a deployment script before a test suite and then pass the deployment results as an environment to the tests. See the section on testing for an example.

Invocation
----------

Much like with the :ref:`compilation phase <compiling>`, the deployment phase is invoked with a minimal PureScript boilerplate.
This script, however, invokes the ``deployScript`` you defined previously, and may either reside with the rest of your source or more
methodically in a separate ``deploy/`` subproject. The latter is demonstrated below

.. code-block:: haskell

   module DeployMain (main) where

   import Prelude
   
   import Chanterelle (deployMain)
   import Control.Monad.Eff (Effect)
      import MyDeployScript (deployScript) as MyDeployScript
   
   main :: Eff Unit
   main = deployMain MyDeployScript.deployScript

We can then invoke this script as follows:

.. code-block:: shell

    pulp build --src-path deploy -I src -m DeployMain --to deploy.js && \
    node deploy.js --log-level info; \
    rm -f deploy.js

One may note the similarities to the invocation of the compiler script, however the build has an additional ``-I src`` as your deploy script
will mostly likely depend on artifacts that are codegen'd into your main source root as well.


Deployer arguments
------------------

Currently the following command line arguments are supported for the deployment phase when ran with ``deployMain``:

- ``--log-level``: One of ``debug``, ``info``, ``warn``, or ``error``. Defaults to ``info``.
  This option changes the level of logging to the console.
