<?php

/**
 * Finds whether an array is associative (true) or numeric (false)
 * @param array $arr
 * @return boolean
 */
function isAssoc($arr){
    if(is_array($arr) && empty($arr)) return false; // empty array is  considered not associative
    
    return array_keys($arr) !== range(0, count($arr) -1);
}

?>