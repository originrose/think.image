package think.image;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.awt.Rectangle;

public class ImageOperations
{
    //Given byte data find a box around all nonzero items
    public static Rectangle byteMaskToRectangle(byte[] byteData, int width, int height)
    {
	int minX = Integer.MAX_VALUE;
	int maxX = Integer.MIN_VALUE;
	int minY = Integer.MAX_VALUE;
	int maxY = Integer.MIN_VALUE;
	for ( int y = 0; y < height; ++y ) {
	    for ( int x = 0; x < width; ++x ) {
		int offset = y * width + x;
		if ( byteData[offset] != 0 ) {
		    minX = Math.min(x, minX );
		    maxX = Math.max(x, maxX );
		    minY = Math.min(y, minY );
		    maxY = Math.max(y, maxY );
		}
	    }
	}
	return new Rectangle( minX, minY, maxX - minX, maxY - minY );
    }
}
