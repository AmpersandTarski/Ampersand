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

    /**
     * Get resource options using API params
     *
     * @param array $params
     * @return integer
     */
    public static function getResourceOptions(array $params): int {
        $optionsMap = ['metaData' => Resource::INCLUDE_META_DATA 
                      ,'sortData' => Resource::INCLUDE_SORT_DATA
                      ,'navIfc' => Resource::INCLUDE_NAV_IFCS
                      ];
        
        return self::processOptionsMap($optionsMap, $params, Resource::DEFAULT_OPTIONS);
    }

    public static function getInterfaceOptions(array $params): int {
        $optionsMap = ['inclLinktoData' => InterfaceObject::INCLUDE_LINKTO_IFCS
                      //,'inclRefIfcs' => InterfaceObject::INCLUDE_REF_IFCS // not a user option!
                      ];

        // Set default
        return self::processOptionsMap($optionsMap, $params, InterfaceObject::DEFAULT_OPTIONS);
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
