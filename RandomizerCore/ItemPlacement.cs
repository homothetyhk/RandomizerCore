namespace RandomizerCore
{
    public readonly struct ItemPlacement
    {
        public readonly RandoItem item;
        public readonly RandoLocation location;
        

        public ItemPlacement(RandoItem item, RandoLocation location)
        {
            this.item = item;
            this.location = location;
        }

        public void Deconstruct(out RandoItem item, out RandoLocation location)
        {
            item = this.item;
            location = this.location;
        }
    }
}
