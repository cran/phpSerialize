<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title>phpSerialize for R Web Sample</title>
</head>

<body>
<?php
  $RFile='phpSerializeWeb.R';
  if (!file_exists($RFile)) die("File $RFile not found");
  $Delta=0.7; # Will be passed to R via environment
  $Rterm='Rterm.exe'; // Add full path info here if not in PATH   
  $pp = popen("$Rterm --no-save  --slave  2>&1 < $RFile DELTA=$Delta","r");
  // Read serialized R output via pipe.
  $sWilcox=fgets($pp);
  $slm=fgets($pp);
  pclose($pp);
  # Display the stuff
#  echo $sWilcox;
#  echo $slm;
  $Wilcox=unserialize($sWilcox);
  $lm=unserialize($slm);
?>
<table  border="1" cellpadding="3" cellspacing="3" bgcolor="#FFFFCC">
  <tr>
    <td>Delta</td>
    <td>p-value Wilcox</td>
  </tr>
  <tr>
    <td><?php echo $Delta?></td>
    <td><?php echo $Wilcox['p.value']?></td>
  </tr>
</table>
<p>
Note: We use <code>$Wilcox['p.value']</code>, because by default <code>simplifyMono=TRUE</code> in phpSerialize.
For <code>simplifyMono=FALSE</code>, use <code>$Wilcox['p.value'][1]</code>. The same holds for <code>sigma</code>
and <code>r.squared</code> in the table below.</p>
<table  border="1" cellpadding="3" cellspacing="3" bgcolor="#FFFF33">
  <tr>
    <td>sigma</td>
    <td>r<sup>2</sup></td>
  </tr>
  <tr>
    <td><?php echo $lm["sigma"]?></td>
    <td><?php echo $lm['r.squared']?></td>
  </tr>
</table>

  <h2>Wilcox asscociative array</h2>
<pre>
<?php
  print_r($Wilcox);
?>
</pre>
  <h2>lm summary asscociative array</h2>
<pre>
<?php
  print_r($lm);
?>
</pre>
</body>
</html>
