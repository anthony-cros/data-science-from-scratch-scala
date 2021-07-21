package scratchscala.utils

import java.io.File
import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

// ===========================================================================
object Images {
 
  def colorMatrix(path: String): Array[Array[Color]] = {          
    val img: BufferedImage = ImageIO.read(new File(path))

    Range(0, img.getHeight)
        .map { i => 
          Range(0, img.getWidth)
            .map { j => new Color(img.getRGB(j, i)) }
            .toArray }
        .toArray      
  }  
  
  // ---------------------------------------------------------------------------
  def rgbTriplet(color: Color): (Int, Int, Int) = // 0..255 each
    ( color.getRed  ,
      color.getGreen,
      color.getBlue )

  // ---------------------------------------------------------------------------
  def rgbValues(color: Color): List[Int] = // 0..255 each
    List(
      color.getRed  ,
      color.getGreen,
      color.getBlue )
      
}

// ===========================================================================
