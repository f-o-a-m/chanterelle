pragma solidity ^0.4.13;

/*
  The ParkingAuthority is the governing contract for ParkingDOA. It holds two important pieces of data--
  the ParkingCSR, which is the crypto-spatial registry contract for all ParkingAnchors, and the user membership
  list, which keeps track of all of the registered users of the DOA. It has the sole authority to extend these
  two registries.
*/

import "zeppelin-solidity/contracts/ownership/Ownable.sol";

import "./ParkingAnchor.sol";
import "./ParkingCSR.sol";
import "./User.sol";

contract ParkingAuthority is Ownable {
    
    ParkingCSR public parkingCSR;
    mapping(address => User) public members;
    
    event RegisteredParkingAnchor(bytes12 csc, address addr, bytes8 geohash);
    event RegisterParkingUser(address accountAddress, address userAddress);

    // Deploy a new parking authority.
    function ParkingAuthority() public Ownable() {
      parkingCSR = new ParkingCSR();
    }

    // A function user to verify a ParkingAnchor, currently mocked.
    function validateParkingAnchor(bytes8 _geohash, bytes32 _anchorId) internal pure returns(bool) {
        return true;
    }

    // Deploy a ParkingAnchor at the given geohash with the given anchorId. Transfer ownership of the
    // anchor to the sender of the transaction.
    function deployParkingAnchor(bytes8 _geohash, bytes32 _anchorId) public onlyOwner() {
        if (validateParkingAnchor(_geohash, _anchorId) == true) {
            ParkingAnchor anchor = new ParkingAnchor(_geohash, _anchorId);
            parkingCSR.register(anchor);
            RegisteredParkingAnchor(anchor.csc(), address(anchor), _geohash);
            anchor.transferOwnership(msg.sender);
        }
    }

    // Create a new user and transer ownership of the account to the message sender.
    function registerUser() public onlyOwner() {
        User newUser = new User();
        newUser.transferOwnership(msg.sender);
        RegisterParkingUser(msg.sender, address(newUser));
    }

    function shouldGiveAccess(User _user, bytes4 _zone) internal pure returns(bool) {
      return true;
    }

    function addZone(bytes4 _zone) public {
      User user = User(msg.sender);
      require(shouldGiveAccess(user, _zone));
      user.addZone(_zone);
    }

}
