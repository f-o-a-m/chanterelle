.. _testing:

=======
Testing
=======

Configuration
-------------

You can use whatever purescript testing framework you want, but for illustrative purposes we will use
`purescript-spec <https://github.com/owickstrom/purescript-spec>`_.

There are various testing utility functions in ``Chanterelle.Test``, probably the most important is ``buildTestConfig``.

.. code-block:: haskell

  type TestConfig r =
    { accounts :: Array Address
    , provider :: Provider
    | r
    }


  buildTestConfig
    :: String
    -> Int
    -> DeployM (Record r)
    -> Aff (TestConfig r)

This function takes in some test configuration options and a deploy script, and outputs a record containing all of the unlocked accounts on the test node, a connection to the node, and whatever the output of your deployment script is. This output is then meant to be threaded through as an environment to the rest of your test suites.

Note, unlike the deploy process meant for actual deployments, ``buildTestConfig`` will not write anything to the file system about the result of your deployment. In other words, test deployments are ephemeral.

Example Test Suite
------------------

Here's an example test suite for our ``SimpleStorage`` contract:

.. code-block:: haskell

  simpleStorageSpec
    :: forall r.
       TestConfig (simpleStorage :: Address | r)
    -> Spec Unit
  simpleStorageSpec {provider, accounts, simpleStorage} = do

    describe "Setting the value of a SimpleStorage Contract" do

      it "can set the value of simple storage" $ do
        var <- makeEmptyVar
        let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
        _ <- forkWeb3 provider $
          event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
            liftEff $ log $ "Received Event: " <> show e
            _ <- liftAff $ putVar cs._count var
            pure TerminateEvent
        let primaryAccount = unsafePartialBecause "Accounts list has at least one account" $ fromJust (accounts !! 0)
            n = unsafePartial fromJust <<< uIntNFromBigNumber s256 <<< embed $ 42
            txOptions = defaultTransactionOptions # _from ?~ primaryAccount
                                                  # _to ?~ simpleStorage
                                                  # _gas ?~ embed 90000
        hx <- assertWeb3 provider $ SimpleStorage.setCount txOptions {_count: n}
        liftEff <<< log $ "setCount tx hash: " <> show hx
        val <- takeVar var
        val `shouldEqual` n

The flow of the test is as follows:

1. We create an ``AVar`` to communicate between the testing thread and the event monitoring thread.
2. We then fork an event monitoring thread for our ``CountSet`` event, placing the first received value in the ``AVar`` and terminating the monitor. Notice that the address for the filter is taken from the supplied ``TestConfig``.
3. We create then our ``TransactionOptions`` and submit a transaction to change the count using the ``setCount`` function from the generated PureScript module.
4. We call ``takeVar`` which blocks until the var is filled, then make sure the value we received is the one we put in.

Admittingly this example is pretty trivial-- of course we're going to get back the value we put in. However, this pattern is pretty universal, namely take the supplied test config to help you template the transactions, call some functions, monitor for some event, then make sure the values are what you want.
