let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/b3ecf8e8e4e1a35ba97fcb7e9f2858d14ee6a912/purs-0.15.7-web3.dhall
        sha256:ce57fd949b7cd331d7c61ff45283e35983dd5797b3f17616dd69f8bc06f54784

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
