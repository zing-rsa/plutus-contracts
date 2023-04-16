import { 
    Blockfrost,
    Data,
    Lucid,
    MintingPolicy,
    PublicKey,
    Unit,
    applyParamsToScript,
    fromText,
    paymentCredentialOf
} from 'https://deno.land/x/lucid@0.9.8/mod.ts'

import keys from './keyfile.json' assert {type: "json"}

const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        keys.blockfrostKey
    ),
    "Preprod"
)

lucid.selectWalletFromSeed(keys.seed)

const Params = Data.Tuple([Data.Bytes()])
type  Params = Data.Static<typeof Params>;

const pkh: PublicKey = paymentCredentialOf(await lucid.wallet.address()).hash

const policy: MintingPolicy = {
    "type": "PlutusV2",
    "script": applyParamsToScript<Params>(
        "5908d25908cf0100003233223322323232323232323232323232323322323232323232322223232533532323232533553353235001222222222222533533355301712001321233001225335002210031001002501e25335333573466e3c0480040c40c04d40800045407c010840c440bd4004408c4cd5ce24914556e617574686f72697a6564206d696e74696e6700022153355335323301f502200135500122222222222200810222213500222253350041333573466e3c009221085a696e67436f696e00028027221029102313357389210e496e76616c696420746f6b656e73000221022135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4060064d5d0a80619a80c00c9aba1500b33501801a35742a014666aa038eb9406cd5d0a804999aa80e3ae501b35742a01066a0300466ae85401cccd54070091d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40b9d69aba15002302f357426ae8940088c98c80c4cd5ce01901881789aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8173ad35742a004605e6ae84d5d1280111931901899ab9c03203102f135573ca00226ea8004d5d09aba2500223263202d33573805c05a05626aae7940044dd50009aba1500533501875c6ae854010ccd540700808004d5d0a801999aa80e3ae200135742a00460446ae84d5d1280111931901499ab9c02a029027135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860246ae84d5d1280211931900d99ab9c01c01b0193333573466e1cd55ce9baa0054800080688c98c8068cd5ce00d80d00c1bae00510181326320183357389210350543500018135573ca00226ea8004c8004d5406888448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448c88c008dd6000990009aa80d111999aab9f0012501a233501930043574200460066ae880080508c8c8cccd5cd19b8735573aa004900011991091980080180118061aba150023005357426ae8940088c98c8050cd5ce00a80a00909aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500d014357426ae8940088c98c8064cd5ce00d00c80b89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403666ae7007006c06406005c4d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201533573802c02a02626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355017223233335573e0044a030466a02e66442466002006004600c6aae754008c014d55cf280118021aba200301213574200224464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900919ab9c01301201000f135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01101000e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00d00c00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00580500409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00a00980880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700340300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801401200e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002c02802001c0184d55cea80089baa0012323333573466e1d40052002200c23333573466e1d40092000200c23263200633573800e00c00800626aae74dd5000a4c240029201035054310032001355006222533500110022213500222330073330080020060010033200135500522225335001100222135002225335333573466e1c005200000c00b1333008007006003133300800733500912333001008003002006003112200212212233001004003122002122001112323001001223300330020020011",
        [pkh],
        Params
    )
}

const zingCoin: Unit = lucid.utils.mintingPolicyToId(policy) + fromText("ZingCoin")

const tx = await lucid
                    .newTx()
                    .addSignerKey(pkh)
                    .attachMintingPolicy(policy)
                    .mintAssets({ [zingCoin]: 100n }, Data.void())
                    .complete();
const signed = await tx.sign().complete();
const TxHash = await signed.submit()


console.log("Submitted: ", TxHash);