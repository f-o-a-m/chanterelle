pragma solidity ^0.4.13;

/*
  The ParkingAuthority is the governing contract for ParkingDOA. It holds two important pieces of data--
  the ParkingCSR, which is the crypto-spatial registry contract for all ParkingAnchors, and the user membership
  list, which keeps track of all of the registered users of the DOA. It has the sole authority to extend these
  two registries.
*/

import "zeppelin-solidity/contracts/ownership/Ownable.sol";

import "./ParkingAnchor.sol";
import "./FoamCSR.sol";
import "./User.sol";

contract ParkingAuthority is Ownable {
    
    FoamCSR public parkingCSR;
    mapping(address => bool) public anchors;
    mapping(address => bool) public users;
    
    event RegisteredParkingAnchor(address owner, address anchor, bytes8 geohash, bytes32 anchorId);
    event RegisterParkingUser(address owner, address user);

    // Decide whether or not to give the user parking access to the zone. Mocked for now.
    modifier shouldGiveAccess(bytes4 _zone) {
        _;
    }

    modifier callerIsUser() {
        require(users[msg.sender]);
        _;
    }

    // Deploy a new parking authority.
    function ParkingAuthority(FoamCSR foamCSR) public Ownable() {
        parkingCSR = foamCSR;
    }

    // A function user to verify a ParkingAnchor, currently mocked.
    function validateParkingAnchor(bytes8 /* _geohash */, bytes32 /* _anchorId */) internal pure returns(bool) {
        return true;
    }

    // Deploy a ParkingAnchor at the given geohash with the given anchorId. Transfer ownership of the
    // anchor to the sender of the transaction.
    function registerParkingAnchor(bytes8 _geohash, bytes32 _anchorId) public {
        require(validateParkingAnchor(_geohash, _anchorId));
        ParkingAnchor anchor = new ParkingAnchor(_geohash, _anchorId);
        parkingCSR.register(anchor);
        anchors[address(anchor)] = true;
        RegisteredParkingAnchor(msg.sender, address(anchor), _geohash, _anchorId);
        anchor.transferOwnership(msg.sender);
    }

    function validateUserApplication(address /* _applicant */) internal pure returns(bool) {
        return true;
    }

    // Create a new user and transer ownership of the account to the message sender.
    function registerUser() public {
        require(validateUserApplication(msg.sender));
        User newUser = new User();
        newUser.transferOwnership(msg.sender);
        users[address(newUser)] = true;
        RegisterParkingUser(msg.sender, address(newUser));
    }

    // This function is called by a user when they want to add a zone to their listed of
    // licensed zones.
    function addZone(bytes4 _zone) public shouldGiveAccess(_zone) callerIsUser() {
        User user = User(msg.sender);
        user.addZone(_zone);
    }

}
