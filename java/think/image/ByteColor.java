package think.image;
import java.awt.image.BufferedImage;
import java.util.Arrays;

public final class ByteColor {
    public static final int A_SHIFT = 24;
    public static final int R_SHIFT = 16;
    public static final int G_SHIFT = 8;
    public static final int B_SHIFT = 0;

    public final byte r;
    public final byte g;
    public final byte b;
    public final byte a;

    public static double toDouble(byte component) {
	return ((component & 0xFF) / 255.0);
    }

    public static double clamp( double value, double min, double max ) {
	return Math.min( Math.max( value, min ), max);
    }

    public static byte toByte(double component) {
	return (byte) clamp (((component * 255.0) + 0.5), 0.0, 255.0);
    }

    public static int componentToInt(byte component, int shift ) {
	return (int) ((int)component & 0xFF) << shift;
    }

    public static int componentToInt(byte component) {
	return (int) componentToInt(component, B_SHIFT);
    }

    public static byte intToComponent(int color, int shift ) {
	return (byte) ((color >> shift) & 0xFF);
    }

    public static byte alphaBlendComponents(byte src, byte dst, double src_mult, double dst_mult) {
	return toByte((toDouble(src) * src_mult) + (toDouble(dst) * dst_mult));
    }

    public ByteColor( int n_r, int n_g, int n_b, int n_a) {
	r = (byte)n_r;
	g = (byte)n_g;
	b = (byte)n_b;
	a = (byte)n_a;
    }

    public ByteColor( int n_r, int n_g, int n_b ) {
	this(n_r, n_g, n_b, 255);
    }

    public ByteColor( int n_r ) {
	this(n_r, n_r, n_r);
    }

    public static int toInt(ByteColor color) {
	return (int)
	    ( componentToInt(color.a, A_SHIFT) |
	      componentToInt(color.r, R_SHIFT) |
	      componentToInt(color.g, G_SHIFT) |
	      componentToInt(color.b, B_SHIFT) );
    }

    public static ByteColor toColor(int int_color) {
	return new ByteColor( intToComponent(int_color, R_SHIFT)
			      , intToComponent(int_color, G_SHIFT)
			      , intToComponent(int_color, B_SHIFT)
			      , intToComponent(int_color, A_SHIFT) );
    }

    public static ByteColor alphaBlend(ByteColor source, ByteColor dest ) {
	double src_mult = toDouble(source.a);
	double dst_mult = 1.0 - src_mult;
	return new ByteColor( alphaBlendComponents(source.r, dest.r, src_mult, dst_mult)
			      , alphaBlendComponents(source.g, dest.g, src_mult, dst_mult)
			      , alphaBlendComponents(source.b, dest.b, src_mult, dst_mult)
			      , (byte) Math.max( source.a, dest.a) );
    }

    public static byte grayScale(ByteColor source) {
	//On the internet, must be true
	return (byte) (componentToInt(source.r) * 0.2989
		       + componentToInt(source.g) * 0.5870
		       + componentToInt(source.b) * 0.1140
		       + 0.5 );
    }

    public static int formatToStride(int format) throws Exception {
	switch(format) {
	case BufferedImage.TYPE_INT_RGB: return 3;
	case BufferedImage.TYPE_INT_ARGB: return 4;
	case BufferedImage.TYPE_BYTE_GRAY: return 1;
	default: throw new Exception( "Failed to convert format" );
	}
    }

    public static ByteColor read(byte[] source, int format, int idx ) throws Exception{
	int offset = 0;
	switch(format) {
	case BufferedImage.TYPE_INT_RGB:
	    offset = idx * 3;
	    return new ByteColor(source[offset], source[offset+1], source[offset+2]);
	case BufferedImage.TYPE_INT_ARGB:
	    offset = idx * 4;
	    return new ByteColor(source[offset], source[offset+1], source[offset+2], source[offset+3]);
	case BufferedImage.TYPE_BYTE_GRAY:
	    return new ByteColor(source[idx]);
	default: throw new Exception( "Unrecognized format" );
	}
    }

    public static ByteColor read(int[] source, int idx ) {
	return toColor(source[idx]);
    }

    public static ByteColor read(ByteColor[] source, int idx ) {
	return source[idx];
    }

    public static ByteColor read(Object source, int format, int idx) throws Exception {
	if (source instanceof byte[])
	    return read( (byte[])source, format, idx );
	else if (source instanceof int[])
	    return read ( (int[])source, idx );
	else
	    return read( (ByteColor[])source, idx );
    }

    public static void write(ByteColor data, byte[] dest, int format, int idx ) throws Exception {
	int offset = 0;
	switch(format) {
	case BufferedImage.TYPE_INT_RGB:
	    offset = idx * 3;
	    dest[offset] = data.r;
	    dest[offset+1] = data.g;
	    dest[offset+2] = data.b;
	    break;
	case BufferedImage.TYPE_INT_ARGB:
	    offset = idx * 4;
	    dest[offset] = data.r;
	    dest[offset+1] = data.g;
	    dest[offset+2] = data.b;
	    dest[offset+3] = data.a;
	    break;
	case BufferedImage.TYPE_BYTE_GRAY:
	    dest[idx] = data.r;
	    break;
	default: throw new Exception( "Unrecognized format" );
	}
    }

    public static void write(ByteColor data, int[] dest, int idx) {
	dest[idx] = toInt(data);
    }

    public static void write(ByteColor data, ByteColor[] dest, int idx) {
	dest[idx] = data;
    }

    public static void write(ByteColor data, Object dest, int format, int idx ) throws Exception {
	if ( dest instanceof byte[] )
	    write( data, (byte[]) dest, format, idx );
	else if (dest instanceof int[] )
	    write( data, (int[])dest, idx );
	else
	    write( data, (ByteColor[])dest, idx );
    }

    public static ByteColor Black = new ByteColor((byte)0);

    public static ByteColor convert( ByteColor lhs, int sourceFormat, int destFormat, ByteColor alphaBlendColor) {
	if ( sourceFormat == destFormat ) {
	    return lhs;
	}
	if ( sourceFormat == BufferedImage.TYPE_INT_ARGB ) {
	    lhs = alphaBlend(lhs, alphaBlendColor);
	}
	if ( destFormat == BufferedImage.TYPE_BYTE_GRAY ) {
	    return new ByteColor(grayScale(lhs));
	}
	return lhs;
    }

    public static int numPixels(Object source, int sourceFormat ) throws Exception {
	if ( source instanceof byte[] ) {
	    byte[] sourceBytes = (byte[]) source;
	    return sourceBytes.length / formatToStride(sourceFormat);
	}
	else if ( source instanceof int[] )
	    return ((int[])source).length;
	else
	    return ((ByteColor[])source).length;
    }

    public static Object convert(Object source, int sourceFormat, Object dest, int destFormat, ByteColor alphaBlendColor) throws Exception {
	int numPix = numPixels(source, sourceFormat);
	for ( int idx = 0; idx < numPix; ++idx ) {
	    ByteColor item = read(source, sourceFormat, idx);
	    item = convert( item, sourceFormat, destFormat, alphaBlendColor);
	    write(item, dest, destFormat, idx);
	}
	return dest;
    }

    public static Object convertAllocate(Object source, int sourceFormat, Class destArrayType, int destFormat
					 , ByteColor alphaBlendColor) throws Exception {
	int numPix = numPixels(source, sourceFormat);
	Object destArray = null;
	if (destArrayType == byte.class) {
	    destArray = new byte[numPix * formatToStride(destFormat)];
	}
	else if ( destArrayType == int.class ) {
	    destArray = new int[numPix];
	}
	else if ( destArrayType == ByteColor.class ) {
	    destArray = new ByteColor[numPix];
	}
	return convert(source, sourceFormat, destArray, destFormat, alphaBlendColor);
    }

    @Override
    public int hashCode() {
	return r ^ b ^ g ^ a;
    }

    @Override
    public boolean equals(Object obj) {
	if ( obj == this )
	    return true;
	if (!(obj instanceof ByteColor))
	    return false;
	ByteColor other = (ByteColor) obj;
	return r == other.r
	    && g == other.g
	    && b == other.b
	    && a == other.a;
    }

    @Override
    public String toString() {
	return String.format( "r:%03d g:%03d b:%03d a:%03d"
			      , componentToInt(r)
			      , componentToInt(g)
			      , componentToInt(b)
			      , componentToInt(a));
    }
}
