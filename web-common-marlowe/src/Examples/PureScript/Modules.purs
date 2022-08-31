module Examples.PureScript.Modules where

import Examples.PureScript.ContractForDifferences as ContractForDifferences
import Examples.PureScript.ContractForDifferencesWithOracle as ContractForDifferencesWithOracle
import Examples.PureScript.CouponBondGuaranteed as CouponBondGuaranteed
import Examples.PureScript.Escrow as Escrow
import Examples.PureScript.EscrowWithCollateral as EscrowWithCollateral
import Examples.PureScript.Swap as Swap
import Examples.PureScript.ZeroCouponBond as ZeroCouponBond
import Language.Marlowe.Extended.V1 (Module)

contractForDifferences :: Module
contractForDifferences = ContractForDifferences.contractModule

contractForDifferencesWithOracle :: Module
contractForDifferencesWithOracle =
  ContractForDifferencesWithOracle.contractModule

couponBondGuaranteed :: Module
couponBondGuaranteed = CouponBondGuaranteed.contractModule

escrow :: Module
escrow = Escrow.contractModule

escrowWithCollateral :: Module
escrowWithCollateral = EscrowWithCollateral.contractModule

swap :: Module
swap = Swap.contractModule

zeroCouponBond :: Module
zeroCouponBond = ZeroCouponBond.contractModule
