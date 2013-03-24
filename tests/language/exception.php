<?php

try {
    throw new Exception('Test');
} catch (Exception $e) {
    echo $e->getMessage() . "\n";
}

