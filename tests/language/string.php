<?php

echo '11';

echo '2\'\'2';

echo '3\\\'3';

echo '\'4';

echo '\\\'5';

echo "66";

echo "77\n88";

echo "99\xA0\$\045";
echo "\n\n\n";

$aze = array ('aa', 'aa' => 'bb');
$zero = 0;
echo "$aze[0]\n";
echo "$aze[aa]\n";
echo "$aze[$zero]\n";
echo "$aze[0][aa]\n";
echo "$zero]\n";


