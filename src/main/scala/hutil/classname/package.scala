package hutil

package object classname {

  def classNameSimple(scalaObject: java.lang.Object): String =
    scalaObject.getClass.getSimpleName

  def className(scalaObject: java.lang.Object): String =
    scalaObject.getClass.getName

  def objectNameSimple(scalaObject: java.lang.Object): String = {
    val cn = scalaObject.getClass.getSimpleName
    cn.substring(0, cn.length() - 1)
  }

  def objectName(scalaObject: java.lang.Object): String = {
    val cn = scalaObject.getClass.getName
    cn.substring(0, cn.length() - 1)
  }
}
