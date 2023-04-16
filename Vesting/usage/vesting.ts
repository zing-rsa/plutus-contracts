import {
    Blockfrost,
    Data,
    Lucid,
    SpendingValidator,
    TxHash,
    getAddressDetails
} from "https://deno.land/x/lucid@0.9.8/mod.ts";
import keys from './keyfile.json' assert {type: "json"}

const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        keys.blockfrostKey
    ),
    "Preprod"
)

const VestingScript: SpendingValidator = {
    "type": "PlutusV2",
    "script": "590b2e590b2b0100003232323233223233223232323232323233223233223232323232323232333222323232322323222323253353232323253355335323232350022235002223500522350022253335333501900b00600215335001153350051333501800b00300710351333501800b00300710351333501800b00300735500322222222222200533501433501635029350052200102c335015502802c123333333300122333573466e1c0080040b80b4894cd4ccd5cd19b8700200102e02d101515335333573466e240080040b80b4404c405088ccd5cd19b8800200102e02d22333573466e240080040b80b488ccd5cd19b8900200102d02e22333573466e200080040b40b8894cd4ccd5cd19b8900200102e02d10011002225335333573466e240080040b80b44008400440ac4cd5ce2491d43616e206f6e6c7920636c61696d20616674657220646561646c696e650002a15335323235002222222222222533533355301a12001321233001225335002210031001002502c25335333573466e3c0380040e80e44d40b8004540b4010840e840e0d401488009400440ac4cd5ce2481144e6f7420616c6c6f77656420746f20636c61696d0002a102a135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd408c090d5d0a80619a8118121aba1500b33502302535742a014666aa04eeb94098d5d0a804999aa813bae502635742a01066a0460606ae85401cccd5409c0c5d69aba150063232323333573466e1cd55cea80124000466a0486464646666ae68cdc39aab9d5002480008cd40a8cd40edd69aba15002303e357426ae8940088c98c810ccd5ce02182082089aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81499a81dbad35742a004607c6ae84d5d1280111931902199ab9c043041041135573ca00226ea8004d5d09aba2500223263203f33573807e07a07a26aae7940044dd50009aba1500533502375c6ae854010ccd5409c0b48004d5d0a801999aa813bae200135742a004605e6ae84d5d1280111931901d99ab9c03b039039135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a008603e6ae84d5d1280211931901699ab9c02d02b02b3333573466e1cd55ce9baa0054800080a88c98c80b0cd5ce0160150151999ab9a3370e6aae7540192000233221233001003002375c6ae854018dd69aba135744a00c464c6405666ae700ac0a40a440a04c98c80a8cd5ce2490350543500028135573ca00226ea80044d55cf280089baa00132001355023221122253350011350032200122133350052200230040023335530071200100500400112223500222350032253335333500800700400215335003100110251024102512223232323253335006215333500621533350082130044984c00d261533350072130044984c00d26100d100b1533350072130044984c00d261533350062130044984c00d26100c1533350052100a100b100915333500521533350072130054984c011261533350062130054984c01126100c100a1533350062130054984c011261533350052130054984c01126100b2533350052153335007215333500721333500b00a002001161616100b153335006215333500621333500a009002001161616100a10092533350042153335006215333500621333500a009002001161616100a1533350052153335005213335009008002001161616100910082533350032153335005215333500521333500900800200116161610091533350042153335004213335008007002001161616100810072533350022153335004215333500421333500800700200116161610081533350032153335003213335007006002001161616100710061235001222222220071222003122200212220011221233001003002122123300100300212212330010030021232230023758002640026aa034446666aae7c004940288cd4024c010d5d080118019aba2002019232323333573466e1cd55cea80124000466442466002006004601c6ae854008c014d5d09aba2500223263201b33573803603203226aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602e6ae854008cd403c058d5d09aba2500223263202033573804003c03c26aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931901119ab9c02202002001f01e135573aa00226ea8004d5d0a80119a805bae357426ae8940088c98c8070cd5ce00e00d00d09aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405c88c8cccd55cf80112804119a8039991091980080180118031aab9d5002300535573ca00460086ae8800c05c4d5d080088910010910911980080200189119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6402e66ae7005c0540540504d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402a66ae7005404c04c0480440404d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6402266ae7004403c03c4d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200f33573801e01a01a26ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201833573803002c02c02a02802602402202026aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900899ab9c01100f00f00e135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8038cd5ce00700600600589aab9d50011375400224464646666ae68cdc3a800a40084a00c46666ae68cdc3a8012400446a010600c6ae84d55cf280211999ab9a3370ea00690001091100111931900799ab9c00f00d00d00c00b135573aa00226ea8004484888c00c010448880048c8cccd5cd19b8750014800880148cccd5cd19b8750024800080148c98c8024cd5ce00480380380309aab9d375400224400424400224002932490350543100112323001001223300330020020011"
}

const VestingDatum = Data.Object({
    beneficiary: Data.Bytes(),
    deadline: Data.Integer()
})
type VestingDatum = Data.Static<typeof VestingDatum>;

async function lockFunds(dtm: VestingDatum, seed: string): Promise<TxHash> {
    lucid.selectWalletFromSeed(seed);

    const scriptAddr = lucid.utils.validatorToAddress(VestingScript)

    const lockingTx = await lucid
                        .newTx()
                        .payToContract(scriptAddr, { inline: Data.to<VestingDatum>(dtm, VestingDatum)}, { lovelace: BigInt(10000000) })
                        .complete()
    const signedTx = await lockingTx.sign().complete()
    const hash = await signedTx.submit();

    return hash;
}

async function claimFunds(dtm: VestingDatum, seed: string): Promise<TxHash> {
    lucid.selectWalletFromSeed(seed);

    const utxoToSpend = (await lucid.utxosAt(lucid.utils.validatorToAddress(VestingScript)))
        .find(u => u.datum == Data.to<VestingDatum>(dtm, VestingDatum));

    if (utxoToSpend !== undefined){
        const consumingTx = await lucid
                                .newTx()
                                .collectFrom([utxoToSpend], Data.void())
                                .attachSpendingValidator(VestingScript)
                                .validFrom(Date.now() - 10000)
                                .addSignerKey(dtm.beneficiary)
                                .complete()
        
        const signed = await consumingTx.sign().complete()
        const hash = await signed.submit();

        return hash;
    } else {
        throw new Error("utxo not found")
    }
}

async function run() {

    const beneficiary = getAddressDetails(await (await Lucid.new(undefined,"Preprod")).selectWalletFromSeed(keys.seed).wallet.address()).paymentCredential?.hash;

    if (beneficiary == undefined) throw new Error("Beneficiary hash not found");

    const deadline = BigInt(Date.now() + 30000)
    console.log("dealine was: ", deadline);

    const dtm = {
        beneficiary,
        deadline
    }

    const lockHash = await lockFunds(dtm, keys.seed);
    console.log("locked: ", lockHash)
    
    // wait 3 min for tx to finalize
    await new Promise((resolve) => setTimeout(resolve, 180 * 1000));
    console.log("waiting 3 min for transaction to propegate...")

    const consumeHash = await claimFunds(dtm, keys.seed);
    console.log("claimed: ", consumeHash)
}

run()
