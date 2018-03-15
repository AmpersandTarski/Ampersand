<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Plugs;

use Ampersand\Core\Atom;
use Ampersand\Interfacing\ViewSegment;

/**
 * Interface for a View plug implementations
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
interface ViewPlugInterface extends PlugInterface
{
    
    /**
     * @param \Ampersand\Interfacing\ViewSegment $view
     * @param \Ampersand\Core\Atom $srcAtom
     * @return array of target atom identifiers
     */
    public function executeViewExpression(ViewSegment $view, Atom $srcAtom): array;
}
