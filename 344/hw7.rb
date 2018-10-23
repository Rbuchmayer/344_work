# preprocess_prog and eval_prog return self unless stated otherwise in a comment

# CSE341, Programming Languages, Homework 7, hw7.rb (see also SML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end

  # checks if v is in between end1 and end2
  def inbetween(v, end1, end2)
    (end1 - GeometryExpression::Epsilon <= v and 
     v <= end2 + GeometryExpression::Epsilon) or
      (end2 - GeometryExpression::Epsilon <= v and
       v <= end1 + GeometryExpression::Epsilon)
  end
  
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y1 - y2).to_f / (x1 - x2)
      b = y2 - m * x2
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  # returns a new point shifted x by dx and y by dy
  def shift(dx, dy)
    Point.new(x + dx, y + dy)
  end

  # returns the intersection with another object
  def intersect obj
    obj.intersectPoint self 
  end

  # intersect double dispatch for Lines
  def intersectLine l
    if real_close(y, l.m * x + l.b)
      self
    else
      NoPoints.new
    end
  end

  # intersect double dispatch for Points
  def intersectPoint p
    if real_close_point(x, y, p.x, p.y)
      self
    else
      NoPoints.new
    end
  end
  # intersect double dispatch for VerticleLines
  def intersectVerticalLine l
    if real_close(x, l.x)
      self
    else
      NoPoints.new
    end
  end

  # intersect double dispatch for LineSegments
  def intersectWithSegmentAsLineResult seg
    if inbetween(x, seg.x1, seg.x2)
      self
    else
      NoPoints.new
    end
  end
  
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  # returns a new Line shifted x by dx units and y by dy units
  def shift(dx, dy)
    Line.new(m, b + dy - m * dx)
  end

  # returns the intersection with another object
  def intersect obj
    obj.intersectLine(self)
  end

  # intersect double dispatch for Points
  def intersectPoint p
    p.intersectLine(self)
  end

  # intersect double dispatch for Lines
  def intersectLine l
    if real_close(m, l.m)
      if real_close(b, l.b)
        self
      else
        NoPoints.new
      end
    else
      @x = (l.b - b) / (m - l.m)
      @y = m * @x + b
      Point.new(@x, @y)
    end
  end

  # intersect double dispatch for VerticleLines
  def intersectVerticalLine l
    Point.new(l.x, m * l.x + b)
  end

  # intersect double dispatch for LineSegments
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  # returns a new VerticleLine shifted x by dx units
  def shift(dx,dy)
    VerticalLine.new(x + dx)
  end

  # returns the intersection with another object
  def intersect obj
    obj.intersectVerticalLine(self)
  end

  # intersect double dispatch for Points
  def intersectPoint p
    p.intersectVerticalLine(self)
  end

  # intersect double dispatch for Lines
  def intersectLine l
    l.intersectVerticalLine(self) 
  end

  # intersect double dispatch for VerticleLines
  def intersectVerticalLine l
    if real_close(x, l.x)
      self
    else
      NoPoints.new
    end
  end

  # intersect double dispatch for LineSegments
  def intersectWithSegmentAsLineResult seg
    seg
  end
  
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  # remember (x1,y1) is to the /right/ of (x2,y2) or /above/ if x1 is real
  # close to x2
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  # returns a Point if the endpoints are the same, else returns
  # a new LineSegment with x1 > x2 or y1 > y2 if x1 and x2 are
  # the same 
  def preprocess_prog
    if real_close_point(x1,y1,x2,y2)
      Point.new(x1, y1)
    elsif (x1 < x2) or (real_close(x1,x2) and y1 < y2)
      LineSegment.new(x2,y2,x1,y1)
    else self
    end
  end

  def eval_prog env
    self
  end

  # returns a new LineSegment with xs shifted by dx and
  # ys shifted by dy
  def shift(dx,dy)
    LineSegment.new(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  end

  # returns the intersection with another object
  def intersect other
    other.intersectLineSegment(self)
  end

  # intersect double dispatch for Points
  def intersectPoint p
    p.intersectLineSegment(self)
  end

  # intersect double dispatch for Lines
  def intersectLine l
    l.intersectLineSegment(self) 
  end

  # intersect double dispatch for VerticleLines
  def intersectVerticalLine l
    l.intersectLineSegment(self) 
  end
  
  # intersect double dispatch for LineSegments
  def intersectWithSegmentAsLineResult seg
    if real_close(x1, x2)
      if y1 < seg.y1
        seg.intersectWithSegmentAsLineResult(self)
      elsif real_close(y2, seg.y1) 
        Point.new(x1, y2)
      elsif y2 > seg.y1
        NoPoints.new
      elsif y2 < seg.y2
        seg
      else
        LineSegment.new(x1, seg.y1, x2, y2)
      end
    else
      if x1 < seg.x1
        seg.intersectWithSegmentAsLineResult(self)
      elsif real_close_point(x2,y2,seg.x2,seg.y2) and
           (not real_close_point(x1,y1,seg.x1,seg.y1)) and
           (not real_close(x2,seg.x2))
        Point.new(x2,y2)
      elsif x2 > seg.x1
        NoPoints.new
      elsif x2 < seg.x2
        seg
      else
        LineSegment.new(seg.x1, seg.y1, x2, y2)
      end
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  # returns the intersection of e1 and e2 evaluated
  def eval_prog env
    (@e1.eval_prog(env)).intersect(@e2.eval_prog(env))
  end

  # returns a new Intersect with e1 and e2 evaluated
  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
end

class Let < GeometryExpression
  
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  # returns a new Let with e1 and e2 evaluated 
  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end

  # evaluated e2 in an environment that includes s and e1
  def eval_prog env
    env_new = Array.new(env)
    @new = env_new.assoc @s
    if @new == nil
      @e2.eval_prog(env_new.push([@s, @e1.eval_prog(env)]))
    else
      @new[1] = @e1.eval_prog(env)
      @e2.eval_prog(env_new)
    end
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end

  def eval_prog env # remember: do not change this method
    x = env.assoc @s
    if x.nil?
      raise "undefined variable"
    end
    x[1]
  end

  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  # returns a new Shift with e evaluated
  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end

  # returns e evaluated and shifted by dx and dy
  def eval_prog env
    @e.eval_prog(env).shift(@dx, @dy)
  end
end
