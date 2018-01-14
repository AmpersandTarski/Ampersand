<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use Ampersand\Interfacing\Resource;
use Ampersand\Interfacing\InterfaceObject;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Options {

    const
    /** Default options */
    DEFAULT_OPTIONS     = 0b00001111,
    
    INCLUDE_META_DATA   = 0b00000001,
    
    INCLUDE_NAV_IFCS    = 0b00000010,
    
    INCLUDE_SORT_DATA   = 0b00000100,

    INCLUDE_REF_IFCS    = 0b00001000,
    
    INCLUDE_LINKTO_IFCS = 0b00011000; // linkto ifcs are ref(erence) interfaces

    /**
     * Get resource options using API params
     *
     * @param array $params
     * @return integer
     */
    public static function getResourceOptions(array $params): int {
        $optionsMap = ['metaData' => self::INCLUDE_META_DATA 
                      ,'sortData' => self::INCLUDE_SORT_DATA
                      ,'navIfc' => self::INCLUDE_NAV_IFCS
                      ];
        
        return self::processOptionsMap($optionsMap, $params, self::DEFAULT_OPTIONS);
    }

    public static function getInterfaceOptions(array $params): int {
        $optionsMap = ['inclLinktoData' => self::INCLUDE_LINKTO_IFCS
                      //,'inclRefIfcs' => self::INCLUDE_REF_IFCS // not a user option!
                      ];

        // Set default
        return self::processOptionsMap($optionsMap, $params, self::DEFAULT_OPTIONS);
    }

    protected static function processOptionsMap(array $optionsMap, array $params, int $result = 0): int {
        foreach ($optionsMap as $option => $value) {
            if(!isset($params[$option])) continue; // Don't change the default setting

            // If true/false => set/unset the option
            $bool = filter_var($params[$option], FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE);
            $result = $bool ? $result | $value : $result & ~$value;
        }
        return $result;
    }
}
