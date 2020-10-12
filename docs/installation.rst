.. _installation:

=========
Installation
=========

Chanterelle exists as both a library and an command-line executable. The library enables
your applications to take advatange of Chanterelle's rapid development pipeline, while the
command-line executable provides easy access to Chanterelle's core functionality.

Installing Globally
------------------

Installing the Chanterelle command-line interface globally allows you to easily access 
Chanterelle's compilation and code generation interface to prevent cyclical dependency
problems when bootstrapping a development environment.

The command-line application first attempts to use a project-local version of Chanterelle
when invoking commands, but has the ability to fall back to a "global" instance, which is
necessary when a project is first created. Chanterelle tries to use a local installation
first to prevent incompatibilities that may occur if there is a mismatch between the global
version and the project-local version.

In order for Chanterelle to be able to fall back to a global installation, one must be
compiled after installing Chanterelle via NPM. This is done via the ``chanterelle global-postinstall`` 
subcommand. 

You can install the HEAD of `master` globally by running:

.. code-block:: shell

    npm install -g f-o-a-m/chanterelle  # Install bleeding-edge Chanterelle CLI
    chanterelle global-postinstall      # Bootstrap the global installation

Or if you would like to install a specific version, you may specify it via NPM:

.. code-block:: shell

    npm install -g f-o-a-m/chanterelle#v3.0.0 # Install Chanterelle CLI v3.0.0
    chanterelle global-postinstall            # Bootstrap the global installation

Installing Locally (per-project)
------------------

You will likely also want to install Chanterelle local to particular project:

.. code-block:: shell

    npm install --save f-o-a-m/chanterelle      # Add the Chanterelle CLI to NPM path for NPM package scripts

If the Chanterelle CLI is invoked within a project containing a local installation that has been compiled,
it will use the version within that project as opposed to the global one.


What is this cyclic dependency stuff all about?
------------------

Chanterelle always first attempts to use the version installed within your project. When you've written a deployment script,
your PureScript compiler will automatically compile the necessary components of Chanterelle to enable it to run as a CLI command
independent of a global installation. This is an important step, as it prevents version mismatches from affecting the integrity of
your deployment scripts. To this end, the Chanterelle CLI command always attempts to first use your project-local copy of Chanterelle,
However, your project needs to compile successfully before that is available. Your deployment script will likely fail to compile as 
it would depend on PureScript bindings to the smart contracts which are being deployed -- bindings which Chanterelle generates -- and
if Chanterelle can't compile because your project can't compile -- then you'd be stuck in an endless cyclic dependency. To resolve this,
Chanterelle allows you to compile an independent global version to fall back to, which would allow you to compile your contracts and
generate any PureScript necessary to proceed with compilation.

Fair enough, but why do I need to run this ``global-postinstall`` subcommand?
------------------

Great question! Chanterelle itself is written in PureScript, and as such it depends on the PureScript compiler. The ``global-postinstall`` merely
compiles the Chanterelle codebase, as it would if you had a project-local version. This is not done as a package postinstall script as, very often in 
global package setups, NPM might not give sufficient permissions to install the PureScript compiler package that Chanterelle depends on or otherwise 
install dependencies. To
maximize the flexibilty of the global installation feature, and avoid running into user-specific permissions Chanterelle separates out this step
such that it is independent of being installed via NPM.

To avoid running into a myriad of permissions issues, we recommend using `NVM <https://github.com/nvm-sh/nvm>` for managing globally available NPM packages.
