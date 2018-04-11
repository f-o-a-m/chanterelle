pragma solidity ^0.4.15;

contract SimpleStorage {
    uint public count;

    event CountSet(uint _count);

    function SimpleStorage(uint _count) public {
        count = _count;
    } 

    function setCount(uint _count) public {
        count = _count;
        CountSet(_count);
    }
}
