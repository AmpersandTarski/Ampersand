<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Interfacing;

use Exception;
use Ampersand\AmpersandApp;
use Ampersand\AngularApp;
use Ampersand\Interfacing\Resource;
use Ampersand\Transaction;
use Ampersand\Log\Logger;
use Ampersand\Log\Notifications;

class InterfaceController {

    /**
     * Reference to the ampersand backend instance
     *
     * @var \Ampersand\AmpersandApp
     */
    protected $ampersandApp;

    /**
     * Reference to the frontend instance
     *
     * @var \Ampersand\AngularApp
     */
    protected $angularApp;

    /**
     * Constructor
     *
     * @param \Ampersand\AmpersandApp $ampersandApp
     * @param \Ampersand\AngularApp $angularApp
     */
    public function __construct(AmpersandApp $ampersandApp, AngularApp $angularApp){
        $this->ampersandApp = $ampersandApp;
        $this->angularApp = $angularApp;
    }

    public function get(Resource $resource, $ifcPath, int $options, $depth) {
        return $resource->walkPath($ifcPath)->get($options, $depth);
    }

    public function put(Resource $resource, $ifcPath, $body, $options, $depth): array {
        $transaction = Transaction::getCurrentTransaction();
        
        // Perform put
        $resource = $resource->walkPath($ifcPath, 'Ampersand\Interfacing\Resource')->put($body)->get($options, $depth);
        
        // Close transaction
        $transaction->close();
        if($transaction->isCommitted()) Logger::getUserLogger()->notice($resource->getLabel() . " updated");
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

        // Return result
        return [ 'content'               => $resource
               , 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'				 => $this->angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
               ];
    }

    public function patch(Resource $resource, $ifcPath, $patches, $options, $depth): array {
        $transaction = Transaction::getCurrentTransaction();
        
        // Perform patch(es)
        $resource = $resource->walkPath($ifcPath, 'Ampersand\Interfacing\Resource')->patch($patches)->get($options, $depth);
        
        // Close transaction
        $transaction->close();
        if($transaction->isCommitted()) Logger::getUserLogger()->notice($resource->getLabel() . " updated");
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
    
        // Return result
        return [ 'patches'               => $patches
               , 'content'               => $resource
               , 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'                 => $this->angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
               ];
    }

    public function post(Resource $resource, $ifcPath, $body, $options, $depth): array {
        $transaction = Transaction::getCurrentTransaction();
        
        // Perform create
        $resource = $resource->walkPath($ifcPath, 'Ampersand\Interfacing\ResourceList')->post($body)->get($options, $depth);
        
        // Close transaction
        $transaction->close();
        if($transaction->isCommitted()) Logger::getUserLogger()->notice($resource->getLabel() . " created");
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
    
        // Return result
        return [ 'content'               => $resource
               , 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'				 => $this->angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
               ];
    }

    /**
     * Delete a resource given an entry resource and a path
     *
     * @param \Ampersand\Interfacing\Resource $resource
     * @param string|array $ifcPath
     * @return array
     */
    public function delete(Resource $resource, $ifcPath): array {
        $transaction = Transaction::getCurrentTransaction();
        
        // Perform delete
        $resource->walkPath($ifcPath, 'Ampersand\Interfacing\Resource')->delete();
        
        // Close transaction
        $transaction->close();
        if($transaction->isCommitted()) Logger::getUserLogger()->notice("Resource deleted");
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
        
        // Return result
        return [ 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'				 => $this->angularApp->getNavToResponse($transaction->invariantRulesHold() ? 'COMMIT' : 'ROLLBACK')
               ];
    }
}
