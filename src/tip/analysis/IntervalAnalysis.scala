package tip.analysis

import tip.cfg._
import tip.ast.AstNodeData._
import tip.ast._
import tip.lattices.IntervalLattice._
import tip.lattices._
import tip.solvers._

class IntervalAnalysis(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData) extends CallStringValueAnalysis(cfg, IntervalLattice) {

  import tip.cfg.CfgOps._
  import tip.ast.AstOps._

  override def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    n match {
      case r: CfgStmtNode =>
        r.data match {
          // Only handle asserts here, everything else can be handled generically by ValueAnalysis
          case AAssertStmt(expr: AExpr, _) =>
            expr match {
              case ABinaryOp(GreatThan, id: AIdentifier, ANumber(i, _), _) =>
                ??? // <--- COMPLETE HERE

              case ABinaryOp(GreatThan, ANumber(i, _), id: AIdentifier, _) =>
                ??? // <--- COMPLETE HERE

              case _ => ???
            }
          case _ => super[CallStringValueAnalysis].localTransfer(n, s)
        }
      case _ => super[CallStringValueAnalysis].localTransfer(n, s)
    }
  }
  
  /**
    * Int values occurring in the program, plus -infinity and +infinity.
    */
  private val B = cfg.nodes.flatMap { n =>
    n.appearingConstants.map { x =>
      IntNum(x.value): Num
    } + MInf + PInf
  }

  def loophead(n: (CallStringContext, CfgNode)): Boolean = {
    n match { // Actually looks at the loop tail, since abstract states are before rather than after node with the propagation solver
      case (_, m) => !(m.succ intersect cfg.dominators(m)).isEmpty
    }
  }

  private def minB(b: IntervalLattice.Num) = B.filter(b <= _).min

  private def maxB(a: IntervalLattice.Num) = B.filter(_ <= a).max

  def widenInterval(x: valuelattice.Element, y: valuelattice.Element): valuelattice.Element =
    (x, y) match {
      case (IntervalLattice.EmptyInterval, _) => y
      case (_, IntervalLattice.EmptyInterval) => x
      case ((l1, h1), (l2, h2)) => ??? //<--- COMPLETE HERE
    }

  def widen(x: liftedstatelattice.Element, y: liftedstatelattice.Element): liftedstatelattice.Element =
    (x, y) match {
      case (liftedstatelattice.Bottom, _) => y
      case (_, liftedstatelattice.Bottom) => x
      case (liftedstatelattice.Lift(xm), liftedstatelattice.Lift(ym)) =>
        liftedstatelattice.Lift(declaredVars.map { v =>
          v -> widenInterval(xm(v), ym(v))
        }.toMap)
    }
}

