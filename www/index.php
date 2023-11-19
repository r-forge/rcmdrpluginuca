<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<!-- MMM
<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>
MMM -->

<h1>[EN] Welcome to RcmdrPlugin.UCA project!</h1>

<p>Some extension to Rcmdr (R Commander) to teach statistics in a first university course made by R-UCA project and used at University of Cadiz (UCA).
This customization includes: quality control graphs, test for randomness, confidence interval and test for sigma for one normal sample and predictions using active model.</p>

<p>Development version of the package also includes new menu entries to hande psychometric data.</p>

<p>All the customizations provides by this package will be propose to be part of the Rcmdr package. All the customization included in the Rcmdr package will be dropped from here.</p>

<center><img width=400 src="https://knuth.uca.es/images/RcmdrPlugin.UCA_quality_control.png"></center>

<p>The package is available on cran, so you can install it as any other package.
The package version in this repository may be newer than the one available in cran. You can install the version available here using the instruction <code>install.packages('RcmdrPlugin.UCA', repos = c('http://R-Forge.R-project.org/', 'https://cran.r-project.org/'), dependencies = TRUE)</code>.</p>

<p>More information in <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>r-forge project summary page</strong></a> or in <a href="http://knuth.uca.es/moodle/course/view.php?id=60&lang=en">project page</a>.</p>

<h1>[ES] &iexcl;Bienvenido al proyecto RcmdrPlugin.UCA!</h1>

<p>Algunas extensiones a Rcmdr (R Commander) para la ense&ntilde;anza de un primer curso universitario en estad&iacute;stica hechas por el proyecto R-UCA y usadas en la Universidad de C&aacute;diz (UCA).
Estas extensiones incluyen: gráficos de control de calidad, test de aleatoriedad, intervalo de confianza y test de hip&oacute;tesis para sigma en una poblaci&oacute;n normal y realizaci&oacute;n de predicciones usando el modelo activo.</p>

<p>La versión de desarrollo incluye opciones de menú para el tratamiento de datos psicométricos.</p>

<p>Todas las adaptaciones ser&aacute;n propuestas para su incorporaci&oacute;n a Rcmdr. Las adaptaciones que se incorporen al paquete Rcmdr se suprimir&aacute;n de este paquete.</p>

<center><img width=400 src="https://knuth.uca.es/images/RcmdrPlugin.UCA_control_calidad.png"></center>

<p>El paquete está disponible en cran, por lo que puede ser instalado como cualquier otro paquete.
La versi&oacute;n del paquete en este repositorio puede ser m&aacute;s reciente que la disponible en cran. Puede instalar la versi&oacute;n disponible aqu&iacute; usando la instrucci&oacute;n <code>install.packages('RcmdrPlugin.UCA', repos = c('http://R-Forge.R-project.org', 'https://cran.r-project.org/'), dependencies = TRUE)</code>.</p>

<p>M&aacute;s informaci&oacute;n en la <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>p&aacute;gina r-forge resumen del proyecto</strong></a> o en la <a href="http://knuth.uca.es/moodle/course/view.php?id=60&lang=es">p&aacute;gina del proyecto</a>.</p>

</body>
</html>
