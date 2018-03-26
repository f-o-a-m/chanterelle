pragma solidity ^0.4.13;

import "./CSR.sol";
import "./CSC.sol";
import "./SpatialUtils.sol";

contract FoamCSR is SpatialUtils {
    mapping(bytes12 => address) CSCRegistry;

    event RegisterCSC(address indexed callerAddress, bytes12 csc, address cscAddress, bytes8 cscGeohash);

    function registry(bytes12 csc) public view returns(address) {
        return CSCRegistry[csc];
    }

    function register(CSC newCsc) public {
        bytes12 csc = newCsc.csc();
        if (CSCRegistry[csc] == 0) {
            address cscAddr = address(newCsc);
            bytes8 geohash = newCsc.geohash();
            bytes12 computedCSC = SpatialUtils.computeCSC(geohash, cscAddr);
            require(computedCSC == csc);
            CSCRegistry[csc] = cscAddr;
            RegisterCSC(msg.sender, csc, cscAddr, geohash);
        }
    }
}