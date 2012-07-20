
package body Geometry.Primitives is
   
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
end Geometry.Primitives;



   
   
   
