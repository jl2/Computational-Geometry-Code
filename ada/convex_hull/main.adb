with Geometry.Primitives;
use Geometry.Primitives;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Sets;

with Ada.Containers.Vectors;

with Ada.Containers;
use Ada.Containers;

with Ada.Text_IO;

with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

procedure Main is
   package TIO renames Ada.Text_IO;
   package FIO is new TIO.Float_IO(Float);
   package NIO is new TIO.Integer_IO(Natural);
   
   package PSP is new Ada.Containers.Ordered_Sets(Point2D);
   package PVP renames PointVect;
   use PSP;
   use PVP;
   
   Point_Set_To_Small : exception;
   -- | 1 lpt1[0] lpt1[1] | 
   -- | 1 lpt2[0] lpt2[1] | < 0 => pt is right of the line between lpt1 and lpt2
   -- | 1  pt[0]   pt[1]  |

   function Is_Right_Of(Pt1, Pt2, Pt : Point2d) return Boolean is
      Det : Float := 
        (Pt2.X*Pt.Y + Pt1.X*Pt2.Y + Pt1.Y*Pt.X) - 
        (Pt2.Y*Pt.X + Pt1.Y*Pt2.X + Pt1.X *Pt.Y);
   begin
      return Det<0.0;
   end Is_Right_Of;
   
   function ConvexHull(Pts: PSP.Set) return PVP.Vector is
      ReturnVal : PVP.Vector := PVP.Empty_Vector;
      PtCursor : PSP.Cursor;
      Lupper : PVP.Vector;
      Llower : PVP.Vector;
   begin
      if PSP.Length(Pts)<2 then
         raise Point_Set_To_Small;
      End if;
      
      if PSP.Length(Pts)<=4 then
         PtCursor := PSP.First(Pts);
         
         while PtCursor /= PSP.No_Element loop
            PVP.Append(ReturnVal, PSP.Element(PtCursor));
            PtCursor := Next(PtCursor);
         end loop;
         return ReturnVal;
      end if;
      
      PtCursor := Pts.First;
      Lupper.Append(Element(Ptcursor),1);
      
      Ptcursor := Next(Ptcursor);
      lupper.Append(Element(Ptcursor),1);
      Ptcursor := Next(Ptcursor);      
      while Ptcursor /= PSP.No_Element loop

         Lupper.Append(Element(Ptcursor), 1);
         Ptcursor := Next(Ptcursor);
         while ((Length(Lupper)>2) and then
                  (not Is_Right_Of(Lupper.Element(Integer(lupper.Length)-2),
                                     Lupper.Element(Integer(lupper.Length)-1),
                                     Lupper.Element(Integer(lupper.Length))))) loop
            declare
               Trm: Integer := Integer(Lupper.Length)-1;
            begin
               PVP.Delete(Lupper, trm, 1);
            end;
            --  Lupper.Delete(Lupper.To_Cursor(), 1);
         end loop;
      end loop;
      
      Ptcursor := Pts.Last;
      Llower.Append(Element(Ptcursor),1);
      
      Ptcursor := Previous(Ptcursor);
      Llower.Append(Element(Ptcursor), 1);
      Ptcursor := Previous(Ptcursor);
      while Ptcursor /= Psp.No_Element loop
         Llower.Append(Element(Ptcursor), 1);
         Ptcursor := Previous(Ptcursor);
         while Length(Llower)>2 and then
           not Is_Right_Of(Llower.Element(Integer(Llower.Length)-2),
                           Llower.Element(Integer(Llower.Length)-1),
                           Llower.Element(Integer(Llower.Length))) loop
            declare
               Trm: Integer := Integer(Llower.Length)-1;
            begin
               PVP.Delete(Llower, trm, 1);
            end;
         end loop;
      end loop;
      
      Llower.Delete(1,1);
      return Lupper & llower;
   end ConvexHull;

   procedure ShowPointVector(Pts : PVP.Vector) is
      PtCursor : PVP.Cursor := PVP.First(Pts);
   begin
      while PtCursor /= PVP.No_Element loop
         PtCursor := Next(PtCursor);
      end loop;
      TIO.New_Line;
   end ShowPointVector;
   function Pt_Vect_From_Set(Pts: Psp.Set) return PVP.Vector is
      PtCursor : PSP.Cursor;
      Rval : PVP.Vector;
   begin
      Ptcursor := Psp.First(Pts);
      while Ptcursor /= Psp.No_Element loop
         PVP.Append(Rval, Element(Ptcursor));
         Ptcursor := Psp.Next(Ptcursor);
      end loop;
      return Rval;
   end Pt_Vect_From_Set;
   function Normal_Distribution(  Seed  : Generator;
                                  Mu : Float := 1.0;
                                  Sigma : Float := 0.5)  return Float is 
   begin
      return
        Mu + (Sigma * Sqrt (-2.0 * Log (Random (Seed), 10.0)) * Cos (2.0 * Pi * Random (Seed)));
   end Normal_Distribution;
      
   Seed         : Generator;
   PtVect : Pvp.Vector;
   
   Pts: PSP.Set := PSP.Empty_Set;
   Hull : PVP.Vector;
   Svg : Svgimg;
   Tmp : Unbounded_String;
   Num_Pts : constant := 50;
begin
   Reset (Seed);
   for I in 1..Num_Pts loop
      Pts.Insert((Normal_Distribution(Seed, 500.0, 250.0),
                  Normal_Distribution(Seed, 500.0, 250.0)));
   end loop;
   Hull := ConvexHull(Pts);
   PolygonVect.Append(Svg.Pgons, From_Pt_Vector(Hull));
   Svg.Pts := Pt_Vect_From_Set(Pts);
   Tmp := To_Svg(Svg);
   Put_Line(Tmp);
end Main;

