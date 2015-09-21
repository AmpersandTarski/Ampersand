<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;

class ExcelImportApi{
	
	/****************************** PARSE FILE ******************************/
	/**
	 * @url POST import
	 */
	public function post(){
		try{
			if (is_uploaded_file($_FILES['file']['tmp_name'])){
				// Parse:
				$parser = new ImportExcel($_FILES['file']['tmp_name']);
				$result = $parser->ParseFile();
				unlink($_FILES['file']['tmp_name']);
			}else{
			    Notifications::addError('No file uploaded');
			}
			
			$result = array('notifications' => $result, 'files' => $_FILES);
			return $result;
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
}
?>