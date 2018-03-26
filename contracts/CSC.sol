pragma solidity ^0.4.13;

import "./SpatialUtils.sol";

/*

  CSC is an abstract class for anything that inherits a spatial coordinate, i.e.
  a geohash. It has the ability to register its location in any Crypto Spatial
  Registery (CSR).

*/

contract CSC is SpatialUtils {

    bytes8 public geohash;
    bytes12 public csc;

    function CSC(bytes8 _geohash) public {
        geohash = _geohash;
        csc = SpatialUtils.computeCSC(geohash, address(this));
    }

}