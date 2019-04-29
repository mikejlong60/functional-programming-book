package chapter8

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop =
    if (p.check && this.check)
      new Prop {
        def check = true
      }
      else {
        new Prop {
          def check = false
        }
      }

}
