pragma solidity ^0.4.13;

/*
  SpatialUtils carries common functions needed across the FOAM contracts.
*/

contract SpatialUtils {
    
    function computeCSC(bytes8 geohash_arg, address addr) public pure returns(bytes12) {
        return bytes12(keccak256(geohash_arg, addr));
    }
}