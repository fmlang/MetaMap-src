package gov.nih.nlm.nls.metamap;

import java.util.List;
import java.util.ArrayList;
import se.sics.prologbeans.*;

/**
 * Utility functions for dealing with Terms.
 *
 * <p>
 * Created: Thu May 21 11:59:49 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class TermUtils {
  public static String getAtomArgument(Term parentTerm, int argnum)
    throws Exception 
  { 
    Term term = parentTerm.getArgument(argnum);
    if (term.isAtom()) {
      return ((PBAtomic)term).toString();
    } else {
      throw new Exception("supplied term is not a string .");
    } 
  }

  public static String getStringArgument(Term parentTerm, int argnum)
    throws Exception 
  { 
    Term term = parentTerm.getArgument(argnum);
    if (term.isString()) {
      return ((PBString)term).toString();
    } else {
      throw new Exception("supplied term is not a string .");
    } 
  }


  public static List<String> getAtomStringListArgument(Term parentTerm, int argnum)
  {
    List<String> stringList = new ArrayList<String>();
    PBList prologList = (PBList)parentTerm.getArgument(argnum);
    for (int i = 1; i <= prologList.getLength(); i++) {
      stringList.add(prologList.getTermAt(i).toString());
    }
    return stringList;
  }

  public static List<String> getStringListArgument(Term parentTerm, int argnum)
  {
    List<String> stringList = new ArrayList<String>();
    Term term = parentTerm.getArgument(argnum);
    PBList prologList = (PBList)term.getArgument(1);
    for (int i = 1; i <= prologList.getLength(); i++) {
      stringList.add(prologList.getTermAt(i).toString());
    }
    return stringList;
  }

  public static int getIntegerArgument(Term parentTerm, int argnum)
    throws Exception
  { 
    Term term = parentTerm.getArgument(argnum);
    if (term.isInteger()) {
      return term.intValue();
    } else {
      throw new Exception("supplied term is not an integer.");
    }
  }

  public static Position getPositionArgument(Term parentTerm, int argnum) 
    throws Exception 
  {
    Term term = parentTerm.getArgument(argnum);
    if (term.isCompound()) 
      return new PositionImpl(term.getArgument(1).intValue(), term.getArgument(2).intValue());
    else 
      throw new Exception("supplied term is not a compound term.");
  }

  public static List<Position> getPositionListArgument(Term parentTerm, int argnum)
  {
    List<Position> posList = new ArrayList<Position>();
    PBList prologList = (PBList)parentTerm.getArgument(argnum);
        for (int i = 1; i <= prologList.getLength(); i++) {
      posList.add(new PositionImpl(prologList.getTermAt(i)));
    }
    return posList;

  }

  public static String getArgumentAsString(Term parentTerm, int argnum)
  {
    Term term = parentTerm.getArgument(argnum);
    return term.toString();
  }

//   public static Term getListElement(Term parentTerm, int elementNum)
//   {
//     if (elementNum == 1) {
//       return parentTerm.getArgument(1);
//     } else {
//       Term cdr = parentTerm.getArgument(2);
//       for (int i = 2; i < elementNum; i++) {
// 	cdr = cdr.getArgument(2);
//       }
//       return cdr.getArgument(1);
//     }
//   }

}
