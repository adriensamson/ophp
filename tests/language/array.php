<?php

$test = array(1, 2, 3);

list($test[0], list($test[1], $test[2])) = array(11, array(21, 22));

echo $test[0] . '-' . $test[1] . '-' . $test[2] . "\n";

