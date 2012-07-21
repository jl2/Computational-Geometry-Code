with Ada.Text_Io;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;

package body Geometry.Primitives is
   package Fio is new Ada.Text_Io.Float_Io(Float);
   
   function "<"(A,B: in Point2D) return Boolean is
   begin
      if A.X /= B.X then
         return A.X<B.X;
      else
         return A.Y<B.Y;
      end if;
   end "<";
   
   function ">"(A,B: in Point2D) return Boolean is
   begin
      if A.X /= B.X then
         return A.X>B.X;
      else
         return A.Y>B.Y;
      end if;
   end ">";
   
   function "="(A,B: in Point2D) return Boolean is
   begin
      return A.X=B.X and A.Y=B.Y;
   end "=";
   
   function Str(Val : in Point2D) return String is
   begin
      return "(" & Float'Image(Val.X) & ", " & Float'Image(Val.Y) & ")";
   end Str;
   
   function "<"(A,B: in LineSeg2D) return Boolean is
   begin
      return False;
   end "<";
   
   function ">"(A,B: in LineSeg2D) return Boolean is
   begin
      return False;
   end ">";
   
   function "="(A,B: in LineSeg2D) return Boolean is
   begin
      return A.Pt1=B.Pt1 and A.Pt2=B.pt2;
   end "=";
   function Str(Val : in LineSeg2D) return String is
   begin
      return "[" & Str(Val.Pt1) & ", " & Str(Val.Pt2) & "]";
   end Str;
   function From_Pt_Vector(Pts : PointVect.Vector) return Polygon is
      Pgon : Polygon;
      Fpt : Point2d;
      Lpt : Point2d;
   begin
      Fpt := PointVect.Element(PointVect.First(Pts));
      Lpt := PointVect.Element(PointVect.last(Pts));
      Pgon.Pts := Pts;
      
      if Fpt /= Lpt then
         PointVect.Append(Pgon.Pts, Fpt);
      end if;
      return Pgon;
   end From_Pt_Vector;
   function To_Svg(Pt : in Point2d) return Ada.Strings.Unbounded.Unbounded_String is
      Rval : Unbounded_String;
      Xstr : String(1..20);
      Ystr : String(1..20);
   begin
      FIO.Put(Xstr, Pt.X, 8,0);
      FIO.Put(Ystr, Pt.Y, 8,0);
      Append(Rval, "<circle cx=""" & Trim(Xstr,Both) & """ cy=""" & Trim(Ystr,Both) & """ r=""3"" stroke=""blue"" stroke-width=""2"" fill=""green""/>");
      return Rval;
   end To_Svg;
   
   function To_Svg(lseg : in LineSeg2D) return Ada.Strings.Unbounded.Unbounded_String is
      Rval : Ada.Strings.Unbounded.Unbounded_String;
      X1s : String(1..20);
      y1s : String(1..20);
      X2s : String(1..20);
      y2s : String(1..20);
   begin
      FIO.Put(X1s, Lseg.Pt1.X,8,0);
      FIO.Put(X2s, Lseg.Pt2.X,8,0);
      FIO.Put(y1s, Lseg.Pt1.Y,8,0);
      FIO.Put(y2s, Lseg.Pt2.Y,8,0);

      Rval := Rval & "<line x1=""" & Trim(X1s,Both) & """ y1=""" & Trim(Y1s,Both) & """ x2="""& Trim(X2s,Both) &""" y2="""& Trim(Y2s,Both) & """ style=""stroke:rgb(99,99,99);stroke-width:2""/>";
      return rval;
   end To_Svg;
   
   function To_Svg(Pgon : in Polygon) return Ada.Strings.Unbounded.Unbounded_String is
      Rval : Ada.Strings.Unbounded.Unbounded_String;
      Xstr : String(1..20);
      ystr : String(1..20);
      Pt : Point2d;
      Pcursor : PointVect.Cursor;
      use Pointvect;
   begin
      Rval := Rval & "<polygon points=""";
      Pcursor := Pointvect.First(Pgon.Pts);
      while Pcursor /= PointVect.No_Element loop
         Pt := Pointvect.Element(Pcursor);
         FIO.Put(Xstr, Pt.X, 8,0);
         FIO.Put(Ystr, Pt.Y, 8,0);
         Rval := Rval & Trim(Xstr, Both) & " " & Trim(Ystr,Both) & " ";
         Pcursor := Pointvect.Next(Pcursor);
      end loop;
      Pt := Element(First(Pgon.Pts));
      FIO.Put(Xstr, pt.X, 8,0);
      FIO.Put(Ystr, pt.Y, 8,0);
      Rval := Rval & Trim(Xstr,Both) & " " & Trim(Ystr, Both);
      Rval := Rval & """ style=""fill:#cccccc; stroke:#000000;stroke-width:1""/>";
      return rval;
   end To_Svg;
   
   function To_Svg(Img : Svgimg) return Ada.Strings.Unbounded.Unbounded_String is
      Rval : Ada.Strings.Unbounded.Unbounded_String;
      PtCursor : PointVect.Cursor;
      LineCursor : LineVect.Cursor;
      PgonCursor : PolygonVect.Cursor;
      use PointVect;
      use LineVect;
      use PolygonVect;
   begin
      Rval := Rval & "<?xml version=""1.0"" standalone=""no""?> <!DOCTYPE svg PUBLIC ""-//W3C//DTD SVG 1.1//EN"" ""http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd""> <svg width=""100%"" height=""100%"" version=""1.1"" xmlns=""http://www.w3.org/2000/svg"">";
      pgoncursor := First(Img.pgons);
      while pgoncursor /= PolygonVect.No_Element loop
         Rval := Rval & To_Svg(Element(Pgoncursor));
         Pgoncursor := Next(Pgoncursor);
      end loop;
      Linecursor := First(Img.lines);
      while linecursor /= LineVect.No_Element loop
         Rval := Rval & To_Svg(Element(linecursor));
         Linecursor := Next(Linecursor);
      end loop;
      Ptcursor := First(Img.Pts);
      while Ptcursor /= PointVect.No_Element loop
         Rval := Rval & To_Svg(Element(Ptcursor));
         Ptcursor := Next(Ptcursor);
      end loop;
      return Rval & "</svg>";
   end To_Svg;
      
end Geometry.Primitives;



   
   
   
