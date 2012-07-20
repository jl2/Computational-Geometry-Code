with Geometry.Primitives;
use Geometry.Primitives;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Sets;

with Ada.Containers.Vectors;

with Ada.Containers;
use Ada.Containers;

with Ada.Text_IO;
  
procedure Main is
   package TIO renames Ada.Text_IO;
   package FIO is new TIO.Float_IO(Float);
   package NIO is new TIO.Integer_IO(Natural);
   
   package PSP is new Ada.Containers.Ordered_Sets(Point2D);
   package PVP is new Ada.Containers.Vectors(Positive, Point2D);
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
               TIO.Put_Line("Removing " & Integer'Image(Trm));
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
      TIO.Put_Line("Vector is:");
      while PtCursor /= PVP.No_Element loop
         TIO.Put_Line("   " & Str(PVP.Element(PtCursor)));
         PtCursor := Next(PtCursor);
      end loop;
      TIO.New_Line;
   end ShowPointVector;
   
   Pt1 : Point2D := (0.0, 0.0);
   Pt2 : Point2D := (4.0, 4.0);
   Pt3 : Point2D := (4.0, 0.0);
   Pt4 : Point2D := (0.0, 4.0);
   Pt5 : Point2D := (2.0, 2.0);
   Ls : LineSeg2D := (Pt1, Pt2);
   
   Pts: PSP.Set := PSP.Empty_Set;
   Hull : PVP.Vector;
   
begin
   Pts.Insert(Pt1);
   Pts.Insert(Pt2);
   Pts.Insert(Pt3);
   Pts.Insert(Pt4);
   Pts.Insert(Pt5);
   
   Hull := ConvexHull(Pts);
   
   TIO.Put_Line(Str(Ls));
   TIO.Put_Line("Convex hull is:");
   ShowPointVector(Hull);
end Main;

