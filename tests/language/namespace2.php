<?php

namespace A\B {

class C {
    public function __construct() { echo "C\n"; }
}

$a = new C();

}
namespace A {

$b = new B\C();

$c = new \A\B\C();
}

namespace {

function truc() { echo "truc\n"; }

}

namespace PP {
    truc();
}

namespace PP2 {
function truc() { echo "pas truc\n"; }
truc();
}
