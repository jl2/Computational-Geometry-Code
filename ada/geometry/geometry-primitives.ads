
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
package Geometry.Primitives is

   type Point2D is record
      X : Float;
      Y : Float;
   end record;
   
   function "<"(A,B: in Point2D) return Boolean;
   function ">"(A,B: in Point2D) return Boolean;
   function "="(A,B: in Point2D) return Boolean;
   function Str(Val : in Point2D) return String;

   

   
   type LineSeg2D is record
      Pt1 : Point2D;
      Pt2 : Point2D;
   end record;
   function "<"(A,B: in LineSeg2D) return Boolean;
   function ">"(A,B: in LineSeg2D) return Boolean;
   function "="(A,B: in LineSeg2D) return Boolean;
   function Str(Val : in LineSeg2D) return String;
   
   package PointVect is new Ada.Containers.Vectors(Positive, Point2D);
   type Polygon is record
      Pts : PointVect.Vector;
   end record;
   
   function From_Pt_Vector(Pts : PointVect.Vector) return Polygon;
   
   package PolygonVect is new Ada.Containers.Vectors(Positive, Polygon);
   package LineVect is  new Ada.Containers.Vectors(Positive, LineSeg2D);
   type SvgImg is record
      Pts : PointVect.Vector;
      Pgons : PolygonVect.Vector;
      Lines : LineVect.Vector;
   end record;
   
   function To_Svg(Pt : in Point2D) return Ada.Strings.Unbounded.Unbounded_String;   
   
   function To_Svg(Lseg : in LineSeg2D) return Ada.Strings.Unbounded.Unbounded_String;
   function To_Svg(Pgon : in Polygon) return Ada.Strings.Unbounded.Unbounded_String;
   function To_Svg(Img : Svgimg) return Ada.Strings.Unbounded.Unbounded_String;
   
end Geometry.Primitives;


