<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Options
{

    const
    /** Default options */
    DEFAULT_OPTIONS     = 0b00001111,
    
    INCLUDE_META_DATA   = 0b00000001,
    
    INCLUDE_NAV_IFCS    = 0b00000010,
    
    INCLUDE_SORT_DATA   = 0b00000100,

    INCLUDE_REF_IFCS    = 0b00001000,
    
    INCLUDE_LINKTO_IFCS = 0b00010000;

    /**
     * Get resource options using API params
     *
     * @param array $params
     * @return integer
     */
    public static function getFromRequestParams(array $params): int
    {
        $optionsMap = ['metaData' => self::INCLUDE_META_DATA
                      ,'sortData' => self::INCLUDE_SORT_DATA
                      ,'navIfc' => self::INCLUDE_NAV_IFCS
                      ,'inclLinktoData' => (INCLUDE_REF_IFCS | self::INCLUDE_LINKTO_IFCS) // flag both options
                      //,'inclRefIfcs' => self::INCLUDE_REF_IFCS // not a user option!
                      ];
        
        return self::processOptionsMap($optionsMap, $params, self::DEFAULT_OPTIONS);
    }

    /**
     * Set/unset options based on provided params and options map
     *
     * @param array $optionsMap
     * @param array $params
     * @param integer $options
     * @return integer
     */
    protected static function processOptionsMap(array $optionsMap, array $params, int $options = 0): int
    {
        foreach ($optionsMap as $option => $value) {
            if (!isset($params[$option])) {
                continue; // Don't change the default setting
            }

            // If true/false => set/unset the option
            $bool = filter_var($params[$option], FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE);
            $options = $bool ? $options | $value : $options & ~$value;
        }
        return $options;
    }
}
