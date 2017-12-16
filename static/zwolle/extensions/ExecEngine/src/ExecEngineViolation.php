<?php

namespace Ampersand\Extension\ExecEngine;

use Exception;

class ExecEngineViolation extends \Ampersand\Rule\Violation {
    
    /**
     * Overwrites getViolationMessage() method from Violation class
     * @throws Exception when segment type is unknown
     * @throws Exception when segment expression return more that 1 tgt atom
     * @return string
     */
    public function getViolationMessage(){
        $database = \Ampersand\Database\Database::singleton();
    
        $strArr = array();
        foreach ($this->rule->violationSegments as $segment){
            // text segment
            if ($segment['segmentType'] == 'Text'){
                $strArr[] = $segment['Text'];
                 
            // expressie segment
            }elseif($segment['segmentType'] == 'Exp'){
                // select starting atom depending on whether the segment uses the src of tgt atom.
                $atom = $segment['srcOrTgt'] == 'Src' ? $this->src : $this->tgt;
                
                $rows = array();
                if($segment['expIsIdent']){ 
                    // when segment expression isIdent (i.e. SRC I or TGT I), we don't have to query the database.
                    $rows[] = array('tgt' => $atom->id);
                }else{
                    // quering the expression
                    $atomId = $database->getDBRepresentation($atom);                    
                    $expSQL = str_replace('_SESSION', session_id(), $segment['expSQL']);
                    $query = "SELECT DISTINCT `tgt` FROM ($expSQL) AS `results` WHERE `src` = '{$atomId}'"; // SRC of TGT kunnen door een expressie gevolgd worden
                    $rows = $database->Exe($query);
                }
    
                // returning the result
                if(count($rows) == 0){
                    $strArr[] = '_NULL';
                }else{
                    $str = '';
                    foreach ($rows as $row) $str .= $row['tgt'] . '_AND';
                    $str = substr($str, 0, -4); // strip the last _AND
                    $strArr[] = str_replace(array('{EX}','{php}'), '', $str); // prevent php interpreter by user input. Only allowed as Text segments specified in &-script
                }
    
            // unknown segment
            }else{
                $errorMessage = "Unknown segmentType '{$segment['segmentType']}' in violationSegments of rule '{$this->rule->id}'";
                throw new Exception($errorMessage, 501); // 501: Not implemented
            }
        }
    
        return $this->message = implode($strArr);
    }
}

?>