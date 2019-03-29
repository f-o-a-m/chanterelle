.. _installation:


=========
installing Chanterelle CLI
=========

NOTE: You need to have `node`, `npm` and `bower` already installed.

To use command line interface of chanterelle you need to, pull the repository from github:
.. code-block:: shell
    git pull git@github.com:f-o-a-m/chanterelle.git
then cd into the pulled repository:
.. code-block:: shell
    cd chanterelle
checkout to latest released version:
.. code-block:: shell
    git checkout $(git tag | sort -V | tail -1)
then install all dependencies
.. code-block:: shell
    make install
and build the project:
.. code-block:: shell
    make build
now we have executable `./bin/chanterelle` which we should add to `$PATH` by running:
.. code-block:: shell
    . ./link.sh
it would append line like this `export PATH=$PATH:/path/to/chanterelle/bin/` to `~/.profile` and/or `~/.zshrc` and/or
`~/.bash_profile` (whichever exists) and also update `PATH` in current shell window.

Here is full script for easy copy&paste:
.. code-block:: shell
    git pull git@github.com:f-o-a-m/chanterelle.git
    cd chanterelle
    make install
    make build
    . ./link.sh

Now you can execute `chanterelle --help` to see all supported commands and arguments or `chanterelle COMMAND --help` to see arguments of specific COMMAND!


=========
updating Chanterelle CLI
=========

To update, you need to go the the folder which you have pulled during installation and update your local repository:
.. code-block:: shell
    git fetch --all --tags --prune
then checkout to latest released version:
.. code-block:: shell
    git checkout $(git tag | sort -V | tail -1)

and then update all dependencies and rebuild the project:
.. code-block:: shell
    make install
    make build

if you have any issue try this:
.. code-block:: shell
    make clear
    make install
    make build

Here is full script for easy copy&paste:
.. code-block:: shell
  git fetch --all --tags --prune
  git checkout $(git tag | sort -V | tail -1)
  make install
  make build