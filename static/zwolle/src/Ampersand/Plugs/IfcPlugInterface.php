<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Plugs;
use Ampersand\Core\Atom;
use Ampersand\Interfacing\InterfaceObject;

/**
 * Interface for a InterfaceObject plug implementations
 * 
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface IfcPlugInterface extends PlugInterface {
    
    /**
     * @param InterfaceObject $ifc
     * @param Atom $srcAtom
     * @return mixed
     */
    public function executeIfcExpression(InterfaceObject $ifc, Atom $srcAtom = null);
    
}
