.. _dependencies:

============
Dependencies
============

As the Ethereum ecosystem has not conclusively settled on a Solidity package management structure yet,
we support referencing any modules installed in ``node_modules`` as additional include paths for use
in your Solidity imports. We see the potential for future EthPM 1.0 and 2.0 support as it appears to be the
direction many new Solidity developments are looking towards.

In the `parking-dao <https://github.com/f-o-a-m/parking-dao>`_ example project, we have ``zeppelin-solidity``
as a listed dependency. By listing this dependency, Chanterelle will provide a remapping to the Solidity compiler so that any
imports starting with ``zeppelin-solidity/`` will be fetched from ``/path/to/project/node_modules/zeppelin-solidity/``.

In the future we aim to have a more clever system in place for handling this usage scenario.
