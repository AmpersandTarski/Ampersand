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
use Ampersand\Misc\Config;
use function Ampersand\Misc\getSafeFileName;

class InterfaceController
{

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
    public function __construct(AmpersandApp $ampersandApp, AngularApp $angularApp)
    {
        $this->ampersandApp = $ampersandApp;
        $this->angularApp = $angularApp;
    }

    public function get(Resource $resource, $ifcPath, int $options, $depth)
    {
        return $resource->walkPath($ifcPath)->get($options, $depth);
    }

    public function put(Resource $resource, $ifcPath, $body, $options, $depth): array
    {
        $transaction = Transaction::getCurrentTransaction();
        
        // Perform put
        $resource = $resource->walkPathToResource($ifcPath)->put($body);
        
        // Close transaction
        $transaction->close();
        if ($transaction->isCommitted()) {
            Logger::getUserLogger()->notice($resource->getLabel() . " updated");
        }

        // Get content to return
        try {
            $content = $resource->get($options, $depth);
        } catch (Exception $e) { // e.g. when read is not allowed
            $content = $body;
        }
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles

        // Return result
        return [ 'content'               => $content
               , 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'isCommitted'           => $transaction->isCommitted()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'                 => $this->angularApp->getNavToResponse($transaction->isCommitted() ? 'COMMIT' : 'ROLLBACK')
               ];
    }

    /**
     * Patch resource with provided patches
     * Use JSONPatch specification for $patches (see: http://jsonpatch.com/)
     *
     * @param \Ampersand\Interfacing\Resource $resource
     * @param string|array $ifcPath
     * @param array $patches
     * @param int $options
     * @param int|null $depth
     * @return array
     */
    public function patch(Resource $resource, $ifcPath, array $patches, int $options, int $depth = null): array
    {
        $transaction = Transaction::getCurrentTransaction();
        
        // Perform patch(es)
        $resource = $resource->walkPathToResource($ifcPath)->patch($patches);

        // Close transaction
        $transaction->close();
        if ($transaction->isCommitted()) {
            Logger::getUserLogger()->notice($resource->getLabel() . " updated");
        }

        // Get content to return
        try {
            $content = $resource->get($options, $depth);
        } catch (Exception $e) { // e.g. when read is not allowed
            $content = null;
        }
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
    
        // Return result
        return [ 'patches'               => $patches
               , 'content'               => $content
               , 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'isCommitted'           => $transaction->isCommitted()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'                 => $this->angularApp->getNavToResponse($transaction->isCommitted() ? 'COMMIT' : 'ROLLBACK')
               ];
    }

    public function post(Resource $resource, $ifcPath, $body, $options, $depth): array
    {
        $transaction = Transaction::getCurrentTransaction();
        
        $list = $resource->walkPathToResourceList($ifcPath);

        // Special case for file upload
        if ($list->getIfc()->tgtConcept->isFileObject()) {
            if (is_uploaded_file($_FILES['file']['tmp_name'])) {
                $resource = $list->post((object) []);

                $tmp_name = $_FILES['file']['tmp_name'];
                $originalFileName = $_FILES['file']['name'];

                $dest = getSafeFileName(Config::get('absolutePath') . Config::get('uploadPath'). $originalFileName);
                $relativePath = Config::get('uploadPath') . pathinfo($dest, PATHINFO_BASENAME);
                
                $result = move_uploaded_file($tmp_name, $dest);
                
                if (!$result) {
                    throw new Exception("Error in file upload", 500);
                }
                
                // Populate filePath and originalFileName relations in database
                $resource->link($relativePath, 'filePath[FileObject*FilePath]')->add();
                $resource->link($originalFileName, 'originalFileName[FileObject*FileName]')->add();
            } else {
                throw new Exception("No file uploaded", 500);
            }
        } else {
            // Perform create
            $resource = $list->post($body);
        }

        // Close transaction
        $transaction->close();
        if ($transaction->isCommitted()) {
            if ($result) {
                Logger::getUserLogger()->notice("File '{$originalFileName}' uploaded");
            } else {
                Logger::getUserLogger()->notice($resource->getLabel() . " created");
            }
        } else {
            // TODO: remove uploaded file
        }

        // Get content to return
        try {
            $content = $resource->get($options, $depth);
        } catch (Exception $e) { // e.g. when read is not allowed
            $content = $body;
        }
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
    
        // Return result
        return [ 'content'               => $content
               , 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'isCommitted'           => $transaction->isCommitted()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'                 => $this->angularApp->getNavToResponse($transaction->isCommitted() ? 'COMMIT' : 'ROLLBACK')
               ];
    }

    /**
     * Delete a resource given an entry resource and a path
     *
     * @param \Ampersand\Interfacing\Resource $resource
     * @param string|array $ifcPath
     * @return array
     */
    public function delete(Resource $resource, $ifcPath): array
    {
        $transaction = Transaction::getCurrentTransaction();
        
        // Perform delete
        $resource->walkPathToResource($ifcPath)->delete();
        
        // Close transaction
        $transaction->close();
        if ($transaction->isCommitted()) {
            Logger::getUserLogger()->notice("Resource deleted");
        }
        
        $this->ampersandApp->checkProcessRules(); // Check all process rules that are relevant for the activate roles
        
        // Return result
        return [ 'notifications'         => Notifications::getAll()
               , 'invariantRulesHold'    => $transaction->invariantRulesHold()
               , 'isCommitted'           => $transaction->isCommitted()
               , 'sessionRefreshAdvice'  => $this->angularApp->getSessionRefreshAdvice()
               , 'navTo'                 => $this->angularApp->getNavToResponse($transaction->isCommitted() ? 'COMMIT' : 'ROLLBACK')
               ];
    }
}
