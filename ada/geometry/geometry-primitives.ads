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
   
end Geometry.Primitives;


