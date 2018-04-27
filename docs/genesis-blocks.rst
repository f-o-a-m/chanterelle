.. _genesis-blocks:

==============
Genesis Blocks
==============

Chanterelle supports generating genesis blocks for use with custom blockchains which depend on externally deployed libraries on other
chains. It utilizes the ``libraries`` field in the :ref:`chanterelle.json <chanterelle-json>`.

The genesis generator takes the path to a ``geth``-compatible genesis block as input (relative to the project root
or absolute), and writes the filled-in block to the output path specified, overwriting it if it exists.

As was seen in the compilation example, the genesis generator is invoked with ``runGeneratorMain``, customarialy as part
of your compilation phase.

As will be noted in the :ref:`libraries <libraries>` chapter, the genesis generator is unable to run if any Fixed libraries
are specified in the project spec.

We typically recommend using the genesis generator in conjunction with `Cliquebait <https://github.com/f-o-a-m/cliquebait>`_.