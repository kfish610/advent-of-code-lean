import Mathlib.Data.Nat.Dist
import Std.Data.HashMap.Basic

variable {α β : Type}

def testing_lists := (
  [3, 4, 2, 1, 3, 3],
  [4, 3, 5, 3, 9, 3]
)

def double (f : α → α → β) (x : α) : β := f x x

def run [ToString α] (f : List Nat × List Nat → α): IO Unit :=
  open IO IO.FS in do
    let text  ← lines "./data/Day1/input.txt"
    let split := text.map (·.splitOn "   ")
    let parts := split.map (fun a => (a[0]!, a[1]!))
    let pairs := parts.map (double ·.map String.toNat!)
    let lists := pairs.toList.unzip
    let sol   := f lists
    println sol

namespace First
  def solve (lists : List Nat × List Nat): Nat :=
    double lists.map List.mergeSort |>
      List.zip.uncurry |>
      List.map (Nat.dist.uncurry) |>
      List.foldl Nat.add 0

  #eval run solve
end First

namespace Second
  def counts (list : List Nat) : Std.HashMap Nat Nat :=
    list.foldl (fun m p => m.insert p (m.getD p 0).succ) Std.HashMap.empty

  def solve (lists : List Nat × List Nat) : Nat :=
    let counts  := counts lists.2
    let weights := lists.1.map (fun n => n * (counts.getD n 0))
    weights.foldl Nat.add 0

  #eval run solve
end Second
