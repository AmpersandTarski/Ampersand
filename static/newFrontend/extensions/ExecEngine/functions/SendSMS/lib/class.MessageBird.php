<?php
/**
 * =======================================================================
 *  File:        class.Messagebird.php
 *  Created:     10-01-2010
 *  Author:      MessageBird B.V.
 *  Version:     v1.2 - 04-03-2013
 *
 *  More information? Go to www.messagebird.com/sms-api
 * ========================================================================
 *  Possible returns:
 * ========================================================================
 * 01 - Request has been processed successfully
 * 70 - An incorrect timestamp notation has been used
 * 72 - The message is too long
 * 89 - Invalid sender
 * 93 - One or several receivers are invalid
 * 95 - No message has been selected
 * 96 - The number of credits is insufficient
 * 97 - Invalid username and/or password
 * 98 - Your ip address is not authorized - based on this account
 * 99 - Cannot connect to the server
 */

/**
 * Class MessageBird
 *
 */
class MessageBird
{
    /**
     * @var string
     */
    protected $username;

    /**
     * @var string
     */
    protected $password;

    /**
     * @var integer/string $sender Can be an number (16 numbers) or an text (11 characters)
     */
    protected $sender = null;

    /**
     * @var array $destination Holds one or more recipients
     */
    protected $destination = array ();

    /**
     * @var integer $reference The reference to identify delivery reports using this reference and the destinations
     */
    protected $reference = null;

    /**
     * @var string $responseType Could be XML, PLAIN or SIMPLE. Determines which kind of response the server will send
     */
    protected $responseType = 'XML';

    /**
     * @var bool $inbox If the messages needs to be send from a MessageBird mobile number
     */
    protected $inbox = false;

    /**
     * @var bool $replacechars Replace non GSM-7 characters by appropriate valid GSM-7 characters
     */
    protected $replacechars = true;

    /**
     * @var string $dlrUrl If you want a dlr notification of the message send to another url then that you have set on the web site, you can use this parameter.
     */
    protected $dlrUrl = false;

    /**
     * @var bool $test Tells if the messages is a test
     */
    protected $test = false;

    /**
     * @var integer/string $timestamp Holds the timestamp to schedule a message, instead of sending it now
     */
    protected $timestamp = null;

    protected $apiResponseCode;
    protected $apiResponseMessage;
    protected $apiCreditBalance;

    /**
     * This constructor sets both username and password
     *
     * @param String $username Username at MessageBird
     * @param String $password Password at MessageBird
     */
    public function __construct($username, $password)
    {
        $this->username = $username;
        $this->password = $password;
    }

    /**
     * Adds MSISDN to the destination array
     *
     * @param Integer $destination The destination MSISDN (Mobile number)
     */
    public function addDestination($destination)
    {
        $this->destination[] = $destination;
    }

    /**
     * Sets the reference linked to the MSISDN so the correct status can be retrieved later.
     *
     * @param Integer $reference An unique reference so delivery reports can be linked to the correct message and MSISDN
     */
    public function setReference($reference)
    {
        $this->reference = (int) $reference;
    }

    /**
     * Sets the sender. This can be an MSISDN (Mobile number) or an Text.
     * When it is only numbers it can be 16 numbers, when it is text, it can only be 11 characters long.
     *
     * @param string /integer $sender The sender of the message which the recipient will see.
     */
    public function setSender($sender)
    {
        $this->sender = $sender;
    }

    /**
     * Sets the date and time when the message should be sent.
     * NOTE: This should be in the timezone that is configured in your settings on the website.
     *
     * possible values for dateTime:
     * instance of DateTime class
     * unix time stamp  -   time()
     * timestamp        -   date('YmdHi')
     * date             -   date('Y-m-d H:i')
     *
     * @param              integer /DateTime $dateTime
     * @param DateTimeZone $timeZone
     */
    public function setTimestamp($dateTime, DateTimeZone $timeZone = null)
    {
        if (! ($dateTime instanceof DateTime)) {
            $dateTime = new DateTime($dateTime, $timeZone);
        }

        if ($timeZone) {
            $dateTime->setTimezone($timeZone);
        }

        // Our API needs the timestamp in YearMonthDayHourMinute so we convert it to this format
        $this->timestamp = $dateTime->format('YmdHi');
    }

    /**
     * Sets the response type to be used for retrieveing the response in specific manner.
     * You can change the response type to anything which is in the API Documentation.
     *
     * @param String $responseType Could be XML, PLAIN or SIMPLE (Default: XML)
     */
    public function setResponseType($responseType)
    {
        $this->responseType = $responseType;
    }

    /**
     * If you want the sender to be an MessageBird mobile number, set this to TRUE
     *
     * @param boolean $inbox
     */
    public function setInbox($inbox)
    {
        if ($inbox === true) {
            $this->inbox = true;
        } else {
            $this->inbox = false;
        }
    }

    /**
     * If you want a dlr notification of the message send to another url then that you have set on the web site, you can use this parameter.
     *
     * @param $dlrUrl
     *
     * @throws Exception
     */
    public function setDlrUrl($dlrUrl)
    {
        if (filter_var($dlrUrl, FILTER_VALIDATE_URL) === false) {
            throw new Exception('$dlrUrl expected a valid URL.');
        }
        $this->dlrUrl = $dlrUrl;
    }

    /**
     *
     * If $replacechars is true, then characters that are not listed in the GSM-7 character set are replaced by alternative characters.
     * If $replacechars is false, then no characters are converted into alternative characters, if there are any characters that are not listed in the GSM-7 character set, the message is sent as UTF-8 (unicode)
     *
     * @param boolean $replacechars
     *
     * @throws Exception
     */
    public function setReplacechars($replacechars)
    {
        if (is_bool($replacechars) === false) {
            throw new Exception('$replacechars expected a boolean.');
        }

        if ($replacechars === true) {
            $this->replacechars = true;
        } else {
            $this->replacechars = false;
        }
    }

    /**
     * If $test is TRUE, then the message is not actually sent or scheduled, and there will be no credits deducted.
     * Validation of the message will take place, and you will also receive a normal response back from the API.
     *
     * @param boolean $test
     */
    public function setTest($test)
    {
        if ($test === true) {
            $this->test = true;
        } else {
            $this->test = false;
        }
    }

    /**
     * Will actualy send the given message to the destinations given using addDestination()
     *
     * @param String $message The message which should be sent to the added destinations.
     */
    public function sendSms($message)
    {
        $destination = implode(',', $this->destination);

        $postParams = array (
            'username'     => $this->username,
            'password'     => $this->password,
            'destination'  => $destination,
            'responsetype' => $this->responseType,
            'sender'       => $this->sender,
            'body'         => $message,
        );

        // If there is a reference set, add it to the parameters
        if ($this->reference !== null) {
            $postParams['reference'] = $this->reference;
        }

        // If there is a timestamp set, add it to the parameters
        if ($this->timestamp !== null) {
            $postParams['timestamp'] = $this->timestamp;
        }

        // If we want the SMS to be send from a MessageBird mobile number
        if ($this->inbox !== false) {
            $postParams['inbox'] = 'true';
        }

        // If we do not want to replace characters
        if ($this->replacechars === false) {
            $postParams['replacechars'] = 'false';
        }

        // If we want to add a DLR url
        if ($this->dlrUrl) {
            $postParams['dlr_url'] = $this->dlrUrl;
        }

        // If we want to send a test message, we set this parameter.
        if ($this->test !== false) {
            $postParams['test'] = 'true';
        }

        // urlencode/concatinate all the paramters using http_build_query()
        $postData = http_build_query($postParams, '', '&');

        $result = $this->sendToHost('api.messagebird.com', '/api/sms', $postData);
        list($headers, $xml) = preg_split("/(\r?\n){2}/", $result, 2);
        $this->XMLtoResult($xml);
    }

    /**
     * Send data to Host
     *
     * @param $host
     * @param $path
     * @param $postData
     *
     * @return string
     */
    protected function sendToHost($host, $path, $postData)
    {
        $fp  = @fsockopen($host, 80);
        $buf = '';
        if ($fp) {
            @fputs($fp, "POST $path HTTP/1.1\r\n");
            @fputs($fp, "Host: $host\r\n");
            @fputs($fp, "Content-type: application/x-www-form-urlencoded\r\n");
            @fputs($fp, "Content-length: " . strlen($postData) . "\r\n");
            @fputs($fp, "Connection: close\r\n\r\n");
            @fputs($fp, $postData);
            while (! @feof($fp)) {
                $buf .= @fgets($fp, 128);
            }
            @fclose($fp);
        }

        return $buf;
    }

    /**
     * Save XML parameters to strings
     *
     * @param String $xml
     */
    protected function XMLtoResult($xml)
    {
        // Newer version of the libXML can be speed-up by this extra setting.
        if (LIBXML_VERSION >= 20621) {
            $xmlOptions = LIBXML_COMPACT;
        } else {
            $xmlOptions = null;
        }
        $data = simplexml_load_string($xml, null, $xmlOptions);

        $this->apiResponseCode    = $data->item->responseCode;
        $this->apiResponseMessage = $data->item->responseMessage;
        $this->apiCreditBalance   = $data->item->credits;
    }

    /**
     * Will return the response code which is returned after sending the the message.
     *
     * @return String The response code
     */
    public function getResponseCode()
    {
        return $this->apiResponseCode;
    }

    /**
     * Will return the response message.
     *
     * @return String The response message
     */
    public function getResponseMessage()
    {
        return $this->apiResponseMessage;
    }

    /**
     * Will return the current credit balance left after sending the messages.
     *
     * @return integer The current credit balance after sending the message. (0 when an error occurred)
     */
    public function getCreditBalance()
    {
        return $this->apiCreditBalance;
    }
}