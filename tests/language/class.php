<?php

class Test {
    const MY_CONST = 'Plop';

    public $test = 'test';
    public static $hello = 'hello';
    private static $truc = 'truc';
    
    public function hello($word) {
        echo "hello ".$word."\n";
    }
    
    public static function sayHello() {
        echo Test::$hello . "\n";
    }
    
    public static function lateHello() {
        echo static::$hello . "\n";
    }
    
    public static function echoTruc() {
        echo self::$truc . "\n";
    }
}

$obj = new Test();

echo $obj->test;
echo "\n";

$obj->test = 'TEST';

echo $obj->test;
echo "\n";

$obj->hello('world');

Test::sayHello();

class Test2 extends Test {
    public static $hello = 'HELLO';
    private static $truc = 'TRUC';
    
    public function hello($word) {
        echo "-----\n";
        parent::hello($word);
        echo "-----\n";
    }
    
    public static function echoTruc() {
        parent::echoTruc();
        echo self::$truc."\n";
    }
    
    public function sayTest() {
        echo $this->test . "\n";
    }
}
echo "Test2\n";
$obj2 = new Test2();
$obj2->hello('world');
$obj2->sayTest();
Test2::sayHello();
Test2::lateHello();
Test2::echoTruc();

echo Test2::MY_CONST . "\n";


function hello(Test $test, $word) {
    $test->hello($word);
}

hello($obj2, 'Type Hint');

